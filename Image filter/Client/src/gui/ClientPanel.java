package gui;

import data.Repository;
import main.Query;
import main.ServerHandler;

import javax.swing.*;
import javax.swing.table.DefaultTableModel;
import java.awt.*;
import java.awt.event.*;
import java.awt.image.BufferedImage;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CopyOnWriteArrayList;

import main.Image;

public class ClientPanel extends JPanel implements Runnable {

    public ClientPanel(JFrame frame, int port) {
        this.frame = frame;
        this.currentImageIndex = -1;
        setPreferredSize(frame.getSize());
        setFocusable(true);
        requestFocus();
        setLayout(new GridBagLayout());
        connectToServer(port);
        while (!serverHandler.isConnected()) {}
        init();
    }

    private void connectToServer(int port) {
        serverHandler = new ServerHandler("", port);
        new Thread(serverHandler).start();
        queries = serverHandler.getRepository().getQueriesQueue();
        images = serverHandler.getRepository().getImagesArrayList();
        queries.add(new Query(Query.FILTERS_LIST_CODE));
        frame.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                int option = JOptionPane.showConfirmDialog(
                        frame,
                        "Are you sure you want to close the application?",
                        "Close Confirmation",
                        JOptionPane.YES_NO_OPTION,
                        JOptionPane.QUESTION_MESSAGE);
                if (option == JOptionPane.YES_OPTION) {
                    queries.add(new Query(Query.DISCONNECTED_CLIENT_CODE));
                    System.exit(0);
                }
            }
        });
    }

    private void init() {
        ButtonListener buttonListener = new ButtonListener();
        GridBagConstraints gbc;

        /* "Cancel" Button */
        JButton cancelingButton = new JButton("Cancel");
        cancelingButton.setActionCommand(Query.CANCELED_TASK_ID_CODE);
        cancelingButton.addActionListener(buttonListener);
        gbc = new GridBagConstraints(2, 3, 1, 1, 23, 8,
                GridBagConstraints.CENTER, GridBagConstraints.BOTH,
                new Insets(10,10,30,30), 10, 10);
        add(cancelingButton, gbc);

        /* Filters list */
        filters = serverHandler.getFiltersList();
        dropDownListBox = new JComboBox<>(filters);
        gbc = new GridBagConstraints(1, 2, 1, 1, 22, 8,
                GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL,
                new Insets(10, 10, 30, 10), 10, 10);
        add(dropDownListBox, gbc);

        /* Text field */
        textField = new JTextField("/home/vladislav/Documents/Development/GitHub/Homework/Third_Semester_SPBU/Image filter/Client/pictures/example_3.jpg", 10);
        gbc = new GridBagConstraints(1, 1, 2, 1, 50, 8,
                GridBagConstraints.CENTER, GridBagConstraints.BOTH,
                new Insets(10, 10, 10, 30), 10, 10);
        add(textField, gbc);

        /* "Send" Button */
        JButton sendingButton = new JButton("Send");
        sendingButton.setActionCommand(Query.IMAGE_SENDING_CODE);
        sendingButton.addActionListener(buttonListener);
        gbc = new GridBagConstraints(2, 2, 1, 1, 23, 8,
                GridBagConstraints.CENTER, GridBagConstraints.BOTH,
                new Insets(10, 10, 30, 30), 10, 10);
        add(sendingButton, gbc);

        /* "Save" Button */
        saveButton = new JButton("Save");
        saveButton.setActionCommand(Query.SAVE_IMAGE_CODE);
        saveButton.addActionListener(buttonListener);
        saveButton.setEnabled(false);
        gbc = new GridBagConstraints(1, 3, 1, 1, 22, 8,
                GridBagConstraints.CENTER, GridBagConstraints.BOTH,
                new Insets(10, 10, 30, 10), 10, 10);
        add(saveButton, gbc);

        /* Images table */
        cells = new Object[][] {};
        tableCellRenderer = new ImageTableCellRenderer(images);
        table = new JTable(new DefaultTableModel(cells, new String[] {"Name", "Progress", "Status"}));
        table.setDefaultRenderer(Object.class, tableCellRenderer);
        table.setRowHeight(50);
        JScrollPane tableScrollPane = new JScrollPane(table);
        gbc = new GridBagConstraints(0, 0, 1, 4, 50, 100,
                GridBagConstraints.CENTER, GridBagConstraints.BOTH,
                new Insets(30,30,30,10), 10, 10);
        add(tableScrollPane, gbc);

        /* Image label */
        imagePanel = new JPanel() {
            @Override
            public void paint(Graphics g) {
                g.drawImage(currentImage, 0, 0, this.getWidth(), this.getHeight(), null);
            }
        };
        gbc = new GridBagConstraints(1, 0, 2, 1, 50, 76,
                GridBagConstraints.NORTHEAST, GridBagConstraints.BOTH,
                new Insets(30,10,10,30), 10, 10);
        add(imagePanel, gbc);
    }

    public void addNotify() {
        super.addNotify();
        if (thread == null) {
            thread = new Thread(this);
            thread.start();
        }
    }

    @Override
    public void run() {
        while (true) {
            int index = tableCellRenderer.getCurrentImageIndex();
            if (currentImageIndex != index || serverHandler.isUpdated()) {
                serverHandler.update();
                currentImageIndex = index;
                updateTable();
                if (currentImageIndex >= 0) {
                    updateGUI();
                }
            }
        }
    }

    private void updateGUI() {
        Image image = images.get(currentImageIndex);
        currentImage = image.getImage();
        if (image.getStatus() != Image.READY) {
            saveButton.setEnabled(false);
        }
        else {
            saveButton.setEnabled(true);
        }
        imagePanel.repaint();
    }

    private void updateTable() {
        int index = 0;
        DefaultTableModel model = (DefaultTableModel) table.getModel();
        for (Image image : images) {
            if (index >= model.getRowCount()) {
                model.addRow(new Object[] {image.getFileName(), image.getPercentWork(), ""});
            }
            else {
                switch (image.getStatus()) {
                    case Image.UPDATED:
                        model.setValueAt(image.getPercentWork(), index, 1);
                        image.setStatus(Image.UNCHANGED);
                        break;
                    case Image.PROCESSED:
                        model.setValueAt(100, index, 1);
                        model.setValueAt("OK", index, 2);
                        image.setStatus(Image.READY);
                        break;
                    case Image.CANCELED:
                        model.setValueAt("Canceled", index, 2);
                        image.setStatus(Image.UNCHANGED);
                        break;
                }
            }
            index++;
        }
    }

    private class ButtonListener implements ActionListener {
        @Override
        public void actionPerformed(ActionEvent event) {
            switch (event.getActionCommand()) {
                case Query.IMAGE_SENDING_CODE:
                    String fileName = textField.getText();
                    int filterID = dropDownListBox.getSelectedIndex();
                    if (!fileName.equals("") && filterID >= 0) {
                        queries.add(new Query(Query.IMAGE_SENDING_CODE, fileName, filterID));
                    }
                    break;
                case Query.CANCELED_TASK_ID_CODE:
                    queries.add(new Query(Query.CANCELED_TASK_ID_CODE));
                    break;
                case Query.SAVE_IMAGE_CODE:
                    fileName = textField.getText();
                    if (!fileName.equals("") && currentImageIndex >= 0) {
                        queries.add(new Query(Query.SAVE_IMAGE_CODE, fileName, currentImageIndex));
                    }
                    break;
            }
            ((JButton) event.getSource()).setEnabled(false);
            new SwingWorker<Void, Void>() {
                @Override
                protected Void doInBackground() throws Exception {
                    Thread.sleep(1000);
                    return null;
                }
                @Override
                protected void done() {
                    try {
                        get();
                    }
                    catch (Exception ignore) {}
                    finally {
                        ((JButton) event.getSource()).setEnabled(true);
                    }
                }
            }.execute();
        }
    }

    private JFrame frame;
    private JComboBox<String> dropDownListBox;
    private JTextField textField;
    private JTable table;
    private JPanel imagePanel;
    private JButton saveButton;
    private BufferedImage currentImage;
    private ImageTableCellRenderer tableCellRenderer;
    private Object[][] cells;

    private ServerHandler serverHandler;
    private ConcurrentLinkedQueue<Query> queries;
    private CopyOnWriteArrayList<Image> images;
    private String[] filters;
    private Thread thread;
    private int currentImageIndex;
}
