package gov.nih.mipav.model.algorithms.itk;


import java.util.*;
import java.io.*;

/** Struct that holds public data about an itk filter.
 * @author Geometric Tools
 */
public class FilterRecordItk implements Serializable
{
    /**
     * Normal filters appear in the jar and the saved record,
     * New filters are only in the jar,
     * Removed filters are obsolete: only in the saved record.
     */
    public enum FilterState {
        NEW ("New"), 
        REMOVED ("Removed"), 
        NORMAL ("");

        private final String m_Name;

        FilterState(String name) {
            this.m_Name = name;
        }

        /** Used to provide the user some info about the filter.
         * @return a description for New or Removed filters.
         */
        String getName() { return m_Name; }

    }

    /**
     * Avoid compiler warning. Change if class fields change.
     */
    static final long serialVersionUID = 4244L;
    /**
     * Name directly from the Itk class, InsightToolkit.itkBinaryDilateImageFilterF2F2 becomes BinaryDilateImage
     */
    public String m_Name;
    /**
     * Is this filter shown in the menu. Default yes, user can turn them off.
     */
    public boolean m_Active;
    /**
     * List of i/o types for the filter. F2F2 is a typical one. 
     */
    public ArrayList<String> m_IOType;
    /**
     * New/removed state. Not serialized. 
     */
    public transient FilterState m_State;

    /**
     * Default constructor, unused.
     */
    public FilterRecordItk() { 
        m_Name = ""; m_Active = false; 
        m_State = FilterState.NORMAL;
        m_IOType = new ArrayList<String>();
    }
   
    /** Create a record, NORMAL filter state.
     * @param name Filter name
     * @param active Show in menu
     */
    public FilterRecordItk(String name, boolean active) {
        m_Name = name;
        m_Active = active;
        m_State = FilterState.NORMAL;
        m_IOType = new ArrayList<String>();
    }
}
