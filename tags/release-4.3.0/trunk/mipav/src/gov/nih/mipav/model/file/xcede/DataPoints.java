package gov.nih.mipav.model.file.xcede;

import java.util.regex.Pattern;

/**
 * A helper class to hold a list of values. The <code>DataPoints</code> is an abstract class, so
 * it can not be instantiated, but you can instantiate the subclass, such as Float, Double and 
 * Integer.
 * @author Hailong Wang, Ph.D
 * @version 1.0, 04/27/2006
 */
public abstract class DataPoints implements Cloneable {
    protected int size = 0;
    protected DataPoints(){
        
    }
    
    /**
     * Returns the point value at the specified position.
     * @param index an index of the position.
     * @return the point value at the specified position.
     */
    public abstract double get(int index);
    
    /**
     * Returns the number of the points
     * @return the number of the points.
     */
    public int size(){
        return size;
    }
    /**
     * Inserts specified value at the specified position in this DataPoints. 
     * @param index index at which the specified element is to be inserted.
     * @param value the value to be inserted.
     */
    public abstract void add(int index, double value);
        
    /**
     * Inserts specified value at the specified position in this DataPoints. 
     * @param index index at which the specified element is to be inserted.
     * @param value the value to be inserted.
     */
    public abstract void add(int index, String value) throws NumberFormatException;
    
    /**
     * The <code>Float</code> classes defines an DataPoints in float precision.
     * @author Hailong Wang, Ph.D
     * @version 1.0, 04/27/2006
     */
    public static class Float extends DataPoints{
        /**
         * Holds the points data.
         */
        private float[] points;
        
        /**
         * Constructs an instance of Float with the specified number of data points.
         * @param size the number of the data points
         */
        public Float(int size){
            this.size = size;
            points = new float[size];
        }
        
        /**
         * @see DataPoints
         */
        public double get(int index){
            return points[index];
        }
        
        /**
         * Constructs the <code>Float</code> class from the string and specified regular
         * expression delimiter.
         * @param s        a string needed to be parsed into Float.
         * @param regex    delimiter in regular expression.
         * @return         the new Float object.
         * @throws NumberFormatException 
         */
        public static Float parse(String s, String regex) throws NumberFormatException{
            if(s == null || s.length() == 0){
                return null;
            }
            if(regex == null || regex.length() == 0){
                regex = " ";
            }
            Pattern p = Pattern.compile(regex);
            String[] fields = p.split(s);
            if(fields == null){
                return null;
            }
            Float dataPoints = new Float(fields.length);
            for(int i = 0; i < fields.length; i++){
                dataPoints.add(i, fields[i]);
            }
            return dataPoints;
        }
        
        /**
         * @see DataPoints
         */
        public void add(int index, double value){
            float fvalue = (float)value;
            points[index] = fvalue;
        }
        
        /**
         * @see DataPoints
         */
        public void add(int index, String value){
            float fvalue = java.lang.Float.parseFloat(value);
            points[index] = fvalue;
        }
        
        /**
         * Clones this <code>Float</code> object.
         */
        public Object clone() {
            Float clone = new Float(size);
            for (int i = 0; i < size(); i++) {
                clone.add(i, get(i));
            }
            return clone;
        }
        
        public String toString(){
            StringBuffer sb = new StringBuffer("");
            for(int i = 0; i < points.length; i++){
                if(i > 0){
                    sb.append(", ");
                }
                sb.append(points[i]);
            }
            return sb.toString();
        }
    }
    
    /**
     * The <code>Double</code> classes defines an DataPoints in double precision.
     * @author Hailong Wang, Ph.D
     * @version 1.0, 04/27/2006
     */
    public static class Double extends DataPoints{
        /**
         * Holds the data points.
         */
        private double[] points;
        
        /**
         * Constructs an instance of Double with the specified number of data points.
         * @param size the number of the data points
         */
        public Double(int size){
            this.size = size;
            points = new double[size];
        }
        
        /**
         * @see DataPoints.
         */
        public double get(int index){
            return points[index];
        }

        
        /**
         * Constructs the <code>Double</code> class from the string and specified regular
         * expression delimiter.
         * @param s        a string needed to be parsed into Double.
         * @param regex    delimiter in regular expression.
         * @return         the new Dobule object.
         * @throws NumberFormatException 
         */
        public static Double parse(String s, String regex) throws NumberFormatException{
            if(s == null || s.length() == 0){
                return null;
            }
            if(regex == null || regex.length() == 0){
                regex = " ";
            }
            Pattern p = Pattern.compile(regex);
            String[] fields = p.split(s);
            if(fields == null){
                return null;
            }
            Double dataPoints = new Double(fields.length);
            for(int i = 0; i < fields.length; i++){
                dataPoints.add(i, fields[i]);
            }
            return dataPoints;
        }
        
        /**
         * @see DataPoints.
         */
        public void add(int index, double value){
            points[index] = value;
        }
        
        /**
         * @see DataPoints.
         */
        public void add(int index, String value){
            double dvalue = java.lang.Double.parseDouble(value);
            points[index] = dvalue;
        }
        
        /**
         * Clones the <code>Double</code> object.
         */
        public Object clone() {
            Double clone = new Double(size);
            for (int i = 0; i < size(); i++) {
                clone.add(i, get(i));
            }
            return clone;
        }
    }
    
    /**
     * The <code>Integer</code> classes defines an DataPoints in integer precision.
     * @author Hailong Wang, Ph.D
     * @version 1.0, 04/27/2006
     */
    public static class Integer extends DataPoints{
        /**
         * data point array.
         */
        private int[] points;

        /**
         * Constructs an instance of Integer with the specified number of data points.
         * @param size the number of the data points
         */
        public Integer(int size){
            this.size = size;
            points = new int[size];
        }
        
        /**
         * @see DataPoints
         */
        public double get(int index){
            return points[index];
        }       
        
        /**
         * Constructs the <code>Integer</code> class from the string and specified regular
         * expression delimiter.
         * @param s        a string needed to be parsed into Integer.
         * @param regex    delimiter in regular expression.
         * @return         the new Integer object.
         * @throws NumberFormatException 
         */
        public static Integer parse(String s, String regex) throws NumberFormatException{
            if(s == null || s.length() == 0){
                return null;
            }
            if(regex == null || regex.length() == 0){
                regex = " ";
            }
            Pattern p = Pattern.compile(regex);
            String[] fields = p.split(s);
            if(fields == null){
                return null;
            }
            Integer dataPoints = new Integer(fields.length);
            for(int i = 0; i < fields.length; i++){
                dataPoints.add(i, fields[i]);
            }
            return dataPoints;
        }
        
        /**
         * @see DataPoints
         */
        public void add(int index, double value){
            int ivalue = (int)value;
            points[index] = ivalue;
        }
        
        /**
         * @see DataPoints
         */
        public void add(int index, String value){
            int ivalue = java.lang.Integer.parseInt(value);
            points[index] = ivalue;
        }
        
        /**
         * Clones the <code>Integer</code> object.
         */
        public Object clone() {
            Integer clone = new Integer(size);
            for (int i = 0; i < size(); i++) {
                clone.add(i, get(i));
            }
            return clone;
        }
    }
}
