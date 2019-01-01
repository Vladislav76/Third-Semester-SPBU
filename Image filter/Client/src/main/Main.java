package main;

import gui.ClientFrame;

import java.awt.*;

public class Main {

    public static void main(String[] args) {
        EventQueue.invokeLater(() -> {
            ClientFrame frame = new ClientFrame(1488);
            frame.setVisible(true);
            frame.setTitle("Image Filter");
        });
    }
}
