package gui;

import javax.swing.*;
import java.awt.*;

public class ClientFrame extends JFrame {

    public ClientFrame(int port) {
        Toolkit kit = Toolkit.getDefaultToolkit();
        Dimension screenSize = kit.getScreenSize();
        setSize((int) (screenSize.width / 1.5), (int) (screenSize.height / 1.5));
        add(new ClientPanel(this, port));
        setLocationRelativeTo(null);
        setResizable(false);
        setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        pack();
    }
}