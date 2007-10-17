package gov.nih.mipav.util;

public class Unit {
    private final String abbrevName;
    private final String fullName;
    
    public Unit(String abbrevName, String fullName){
        this.abbrevName = abbrevName;
        this.fullName = fullName;
    }

    public String getAbbrevName() {
        return abbrevName;
    }

    public String getFullName() {
        return fullName;
    }
}
