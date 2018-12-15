public class Main {

    public static void main(String[] args) {
        ServerHandler serverHandler = new ServerHandler("", 1488);
        new Thread(serverHandler).start();
    }
}
