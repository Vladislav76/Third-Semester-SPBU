package src;

public class Main {

    public static void main(String[] args) {
        for (int i = 0; i < 3; i++) {
            WebCrawler webCrawler = new WebCrawler(4);
            webCrawler.crawl("http://127.0.0.1:8080/phpmyadmin/", 3);
        }
    }
}
