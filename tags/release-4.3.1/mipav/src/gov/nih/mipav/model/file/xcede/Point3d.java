package gov.nih.mipav.model.file.xcede;

import gov.nih.mipav.view.MipavUtil;

import java.util.regex.Pattern;

/**
 * Represents the 3-dimension point.
 * @author Hailong Wang, Ph.D
 * @version 1.0, 04/24/2006
 */
public class Point3d{
    private double x;
    private double y;
    private double z;
    
    public Point3d(){
        this(0, 0, 0);
    }
    
    public Point3d(double x, double y, double z){
        this.x = x;
        this.y = y;
        this.z = z;
    }
    
    /**
     * Parses the (x, y, z) like string and creates the new instance of Point3d
     * @param pointString the string including the triple.
     * @return the new instance of Point3d with the specified x, y, z.
     */
    public static Point3d parse(String pointString, String regex) throws NumberFormatException{
        if(pointString == null || pointString.length() == 0){
            return null;
        }
        Pattern p = Pattern.compile(regex);
        String[] fields = p.split(pointString);
        if(fields == null || fields.length != 3){
            MipavUtil.displayError("The rasorigin of the Datarec element doesn't have the correct value: "+pointString);
            return null;
        }
        Point3d point3d = new Point3d();
        point3d.setX(Double.parseDouble(fields[0]));
        point3d.setY(Double.parseDouble(fields[1]));
        point3d.setZ(Double.parseDouble(fields[2]));
        return point3d;
    }
    
    /**
     * Returns the x coordinate value.
     * 
     * @return the x coordinate value.
     */
    public double getX(){
        return x;
    }
    
    /**
     * Sets the x coordinate value.
     * @param x the new x coordinate value.
     */
    public void setX(double x){
        this.x = x;
    }
    
    /**
     * Returns the y coordinate value.
     * @return the y coordinate value.
     */
    public double getY(){
        return y;
    }
    
    /**
     * Sets the y coordinate value
     * @param y the new y coordinate value.
     */
    public void setY(double y){
        this.y = y;
    }
    
    /**
     * Returns the z coordinate value.
     * @return the z coordinate value.
     */
    public double getZ(){
        return z;
    }
    
    /**
     * Sets the z coordinate value
     * @param z the new coordinate value.
     */
    public void setZ(double z){
        this.z = z;
    }
    
    public String toString(){
        StringBuffer sb = new StringBuffer("");
        sb.append("[");
        sb.append(x);
        sb.append(", ");
        sb.append(y);
        sb.append(", ");
        sb.append(z);
        sb.append("]");
        return sb.toString();
    }
}
