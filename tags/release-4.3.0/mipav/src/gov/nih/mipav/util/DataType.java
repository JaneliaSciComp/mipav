package gov.nih.mipav.util;

public class DataType {
    private final int index;
    private final String name;
    public DataType(int index, String name){
        this.index = index;
        this.name = name;
    }
    public int getIndex() {
        return index;
    }
    public String getName() {
        return name;
    }
}
