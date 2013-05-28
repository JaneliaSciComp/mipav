package gov.nih.mipav.model.algorithms.itk;

import InsightToolkit.*;

//import gov.nih.mipav.model.structures.*;

public class PItkFilter
{
    /** Contains the itk smart pointer, which will delete our itk filter
     * unless we keep it around. The only common base type is Object. 
     */
    private Object m_SmartPointer = null;

    /** image to image filter, common base type. 
     */
    private itkProcessObject m_itkFilter = null;

    /** number of dimensions for this filter's i/o */
    private int m_nDims = 2;

    private String m_baseName = null;

    protected void finalize() {
        m_itkFilter = null;
        m_SmartPointer = null;
    }

    /** Create instance of itkImage, matching Mipav image type, if possible.
     * Wrapper around itk smart pointer and the more useful image pointer. 
     * @param model_image_type as defined by consts in ModelStorageBase 
     */
    public PItkFilter(String base_name, String input_type, String output_type)
    {
        m_baseName = base_name;
        // get last char and try to parse as int.
        try {
            m_nDims = Integer.parseInt(input_type.substring(input_type.length() - 1));
        } catch (NumberFormatException nfe) {
        }

        Class<?> cls = null;
        try {
            cls = Class.forName("InsightToolkit.itk" + base_name + "Filter" + input_type +
                                output_type);
            //msg = "Found "+ cls.getName() + "\n";
        }
        catch (ClassNotFoundException cnfe) {
            // long data type not found, try the shorter version, if i/o types are equal.
        }
        if (cls == null && input_type.equals(output_type)) {
            try {
                cls = Class.forName("InsightToolkit.itk" + base_name + "Filter" + input_type);
            }
            catch (ClassNotFoundException cnfe) {
            }
        }
        if (cls == null) {
            // One more try - sometimes the input type is doubled, so you see F2F2F2
            try {
                cls = Class.forName("InsightToolkit.itk" + base_name + "Filter" + 
                                    input_type + input_type + output_type);
            }
            catch (ClassNotFoundException cnfe) {
            }
        }
        if (cls == null) {
            // we can't find it.
            return;
        }

        // Create a filter object, matching i/o type.
        m_SmartPointer = AutoItkLoader.createFilterObj(cls);
        if (m_SmartPointer == null) return;
        m_itkFilter = (itkProcessObject)AutoItkLoader.invokeMethod("GetPointer", m_SmartPointer);

    }

    /** Access the filter 
     * @return Itk filter. null if filter name can't handle i/o types.
     */
    public itkProcessObject filter() {
        return m_itkFilter;
    }

    public int getNDims() {
        return m_nDims;
    }

    public String getName() {
        return m_baseName;
    }
}
