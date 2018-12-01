package src;

import javafx.util.Pair;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import java.io.*;
import java.net.URL;
import java.util.concurrent.*;

public class WebCrawler {

    public WebCrawler() {
        allPages = new ConcurrentSkipListSet<>();
        notVisitedPages = new ConcurrentLinkedQueue<>();
        executorService = Executors.newSingleThreadExecutor();

        final String folderName = "/home/vladislav/Downloads/Pages/";

        searchAndDownloadTask = () -> {
            while (notVisitedPages.size() > 0) {
                Pair<String, Integer> page = notVisitedPages.poll();
                try {
                    String url = page.getKey();
                    int depth = page.getValue();
                    Document doc = Jsoup.connect(url).get();

                    if (depth < maxDepth) {
                        Elements links = doc.select("a[href]");
                        for (Element link : links) {
                            String pageURL = link.attr("abs:href");
                            if (!allPages.contains(pageURL)) {
                                notVisitedPages.add(new Pair<>(pageURL, depth + 1));
                                allPages.add(pageURL);
                            }
                        }
                    }
                    System.out.println(url + ";");

                    int indexName = url.lastIndexOf("/");
                    if (indexName == url.length() - 1) {
                        indexName = url.substring(0, indexName).lastIndexOf("/");
                    }
                    String name = url.substring(indexName, url.length());

                    InputStream in = new URL(url).openStream();
                    OutputStream out = new BufferedOutputStream(new FileOutputStream(folderName + name));
                    for (int b; (b = in.read()) != -1;) {
                        out.write(b);
                    }
                    out.close();
                    in.close();
                }
                catch (IOException e) {
                    e.printStackTrace();
                }
            }
        };
    }

    public void setThreadsNumber(int threadsNumber) {
        executorService = Executors.newFixedThreadPool(threadsNumber);
    }

    public void crawl(String mainURL, int maxDepth) {
        this.mainURL = mainURL;
        this.maxDepth = maxDepth;
        notVisitedPages.add(new Pair<>(this.mainURL, 1));
        allPages.add(this.mainURL);

        executorService.execute(searchAndDownloadTask);
        executorService.shutdown();
    }

    private String mainURL;
    private int maxDepth;
    private ConcurrentLinkedQueue<Pair<String, Integer>> notVisitedPages;
    private ConcurrentSkipListSet<String> allPages;
    private ExecutorService executorService;
    private Runnable searchAndDownloadTask;
}