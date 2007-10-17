package gov.nih.mipav.model;

import gov.nih.mipav.util.Unit;

public class Resolution {
    private final float value;
    private final Unit unit;
    public Resolution(float value, Unit unit){
        this.value = value;
        this.unit = unit;
    }
    
    public float getValue(){
        return value;
    }
    
    public Unit getUnit(){
        return unit;
    }
}
