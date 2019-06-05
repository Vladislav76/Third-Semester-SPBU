package main;

public class Query {

    public Query(String code) {
        this.code = code;
        this.fileName = null;
        this.id = 0;
    }

    public Query(String code, String fileName, int id) {
        this.code = code;
        this.fileName = fileName;
        this.id = id;
    }

    public String getCode() {
        return code;
    }

    public String getFileName() {
        return fileName;
    }

    public int getID() {
        return id;
    }

    private String code;
    private String fileName;
    private int id;

    public static final String IMAGE_SENDING_CODE = "image";
    public static final String CANCELED_TASK_ID_CODE = "cancel";
    public static final String FILTERS_LIST_CODE = "filters";
    public static final String PROCESSING_PROGRESS_CODE = "progress";
    public static final String DISCONNECTED_CLIENT_CODE = "q";
    public static final String SAVE_IMAGE_CODE = "save";
}
