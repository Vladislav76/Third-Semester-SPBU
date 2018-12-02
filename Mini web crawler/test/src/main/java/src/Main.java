package src;

public class Main {

    public static void main(String[] args) {
        WebCrawler webCrawler = new WebCrawler(8);
        webCrawler.crawl("http://127.0.0.1:8080/phpmyadmin/",  3);
    }
}
