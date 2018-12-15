import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.concurrent.*;

public class Server {

    public Server(int port) {
        this.port = port;
        this.executorService = Executors.newFixedThreadPool(2);
    }

    public void start() {
        try (ServerSocket server = new ServerSocket(port)) {
            while (!server.isClosed()) {
                Socket client = server.accept();
                System.out.println("Connection accepted: " + client);
                new Thread(new ClientHandler(client, executorService)).start();
            }
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    private int port;
    private ExecutorService executorService;

    public static final String[] FILTERS = {"Blur filter", "Negative filter"};
}
