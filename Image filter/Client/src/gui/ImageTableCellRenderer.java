package gui;

import data.Repository;
import main.Image;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.table.TableCellRenderer;
import java.awt.*;
import java.util.concurrent.CopyOnWriteArrayList;

public class ImageTableCellRenderer extends JPanel implements TableCellRenderer {

    public ImageTableCellRenderer(CopyOnWriteArrayList<Image> images) {
        this.images = images;
        nameLabel = new JLabel();
        nameLabel.setHorizontalAlignment(SwingConstants.CENTER);
        statusLabel = new JLabel();
        statusLabel.setHorizontalAlignment(SwingConstants.CENTER);
        progressBar = new JProgressBar(0, 100);
        progressBar.setStringPainted(true);
        progressBar.setForeground(Color.BLUE);
        boldBorder = UIManager.getBorder("Table.focusCellHighlightBorder");
        currentImageIndex = -1;
    }

    public int getCurrentImageIndex() {
        return currentImageIndex;
    }

    @Override
    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
        if (isSelected) {
            nameLabel.setBorder(boldBorder);
            statusLabel.setBorder(boldBorder);
            currentImageIndex = row;
        }
        else {
            nameLabel.setBorder(null);
            statusLabel.setBorder(null);
        }
        switch (column) {
            case FILENAME_COLUMN:
                nameLabel.setText((String) value);
                return nameLabel;
            case PROGRESS_COLUMN:
                progressBar.setValue((int) value);
                return progressBar;
            case STATUS_COLUMN:
                statusLabel.setText((String) value);
                return statusLabel;
        }
        return this;
    }

    private Border boldBorder;
    private JProgressBar progressBar;
    private JLabel nameLabel;
    private JLabel statusLabel;
    private CopyOnWriteArrayList<Image> images;
    private int currentImageIndex;

    private static final int FILENAME_COLUMN = 0;
    private static final int PROGRESS_COLUMN = 1;
    private static final int STATUS_COLUMN = 2;
}
