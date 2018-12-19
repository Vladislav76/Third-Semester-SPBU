import java.io.FileInputStream;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Properties;
import java.util.concurrent.*;

public class Server {

    public Server(int port) {
        this.port = port;
        this.executorService = Executors.newFixedThreadPool(4);
        filters = new String[] {};
    }

    public static String[] getFilters() {
        return filters;
    }

    public void start() {
        try (ServerSocket server = new ServerSocket(port)) {
            loadConfig();
            while (!server.isClosed()) {
                Socket client = server.accept();
                new Thread(new ClientHandler(client, executorService)).start();
            }
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void loadConfig() throws IOException {
        Properties properties = new Properties();
        properties.load(new FileInputStream("src/app.config"));
        filters = properties.get("FILTERS").toString().split("#");
    }

    private int port;
    private ExecutorService executorService;
    private static String[] filters;
}
