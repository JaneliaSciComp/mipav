package gov.nih.mipav.model.algorithms;

import static org.jocl.CL.*;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

import java.awt.Container;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JScrollPane;

import org.jocl.cl_device_id;
import org.jocl.cl_platform_id;


/** Queries the OpenCL Platforms and launches an information dialog with the OpenCL Platform and Device information: */
public class OpenCLInfo {

	/** Queries the OpenCL Platforms and launches an information dialog with the OpenCL Platform and Device information: */
    public static void main(String[] args) {
        
        JFrame frame = new JFrame("OpenCL Info");
        
        try {
        	frame.setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (final FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n");
        }
        
        frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        Container contentPane = frame.getContentPane();

        JEditorPane area = new JEditorPane();
        area.setContentType("text/html");
        area.setEditable(false);

        contentPane.add(new JScrollPane(area));

        String html = createOpenCLInfoHTML();

        area.setText(html.toString());

        frame.setSize(1200, 800);
        frame.setVisible(true);

    }

    /**
     * Creates the table displayed in the information dialog. First column is the platform property name, second (+) columns contain the values
     * for each platform and device.
     * @return string version of the table.
     */
    private static String createOpenCLInfoHTML() {

        StringBuilder html = new StringBuilder();

        html.append("<table border=\"1\">");
        

        // Obtain the number of platforms
        int numPlatforms[] = new int[1];
        clGetPlatformIDs(0, null, numPlatforms);

        System.out.println("Number of platforms: "+numPlatforms[0]);

        // Obtain the platform IDs
        cl_platform_id platforms[] = new cl_platform_id[numPlatforms[0]];
        clGetPlatformIDs(platforms.length, platforms, null);

        // platforms
        Properties temp;
        List<Properties> platProps = new ArrayList<Properties>();
        List<Integer> spans = new ArrayList<Integer>();
        List<String> keys = new ArrayList<String>();
        for (int i=0; i<platforms.length; i++)
        {
            String platformName = OpenCLAlgorithmBase.getString(platforms[i], CL_PLATFORM_NAME);

            // Obtain the number of devices for the current platform
            int numDevices[] = new int[1];
            clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_ALL, 0, null, numDevices);

            System.out.println("Number of devices in platform "+platformName+": "+numDevices[0]);

            cl_device_id devicesArray[] = new cl_device_id[numDevices[0]];
            clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_ALL, numDevices[0], devicesArray, null);

            temp = new Properties();
            temp.setProperty( "Platform", OpenCLAlgorithmBase.getString( platforms[i], CL_PLATFORM_NAME)  ); 
            temp.setProperty( "Vendor", OpenCLAlgorithmBase.getString( platforms[i], CL_PLATFORM_VENDOR)  );
            temp.setProperty( "Profile", OpenCLAlgorithmBase.getString( platforms[i], CL_PLATFORM_PROFILE)  );
            temp.setProperty( "Version", OpenCLAlgorithmBase.getString( platforms[i], CL_PLATFORM_VERSION)  );
            temp.setProperty( "Extensions", OpenCLAlgorithmBase.getString( platforms[i], CL_PLATFORM_EXTENSIONS)  );
            platProps.add(temp);

            
            spans.add(numDevices[0]);
        }
        keys.add( "Platform" );
        keys.add( "Vendor" );
        keys.add( "Profile" );
        keys.add( "Version" );
        keys.add( "Extensions" );
        
        fillTable( keys, platProps, spans, html);

        keys.clear();
        keys.add( "CL_DEVICE_NAME" );
        keys.add( "CL_DEVICE_TYPE" );
        keys.add( "CL_DEVICE_MAX_COMPUTE_UNITS" );
        keys.add( "CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS" );
        keys.add( "CL_DEVICE_MAX_WORK_ITEM_SIZES" );
        keys.add( "CL_DEVICE_MAX_WORK_GROUP_SIZE" );
        keys.add( "CL_DEVICE_MAX_CLOCK_FREQUENCY" );
        keys.add( "CL_DEVICE_ADDRESS_BITS" );
        keys.add( "CL_DEVICE_MAX_MEM_ALLOC_SIZE" );
        keys.add( "CL_DEVICE_GLOBAL_MEM_SIZE" );
        keys.add( "CL_DEVICE_ERROR_CORRECTION_SUPPORT" );
        keys.add( "CL_DEVICE_LOCAL_MEM_TYPE" );
        keys.add( "CL_DEVICE_LOCAL_MEM_SIZE" );
        keys.add( "CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE" );
        keys.add( "CL_DEVICE_QUEUE_PROPERTIES" );
        keys.add( "CL_DEVICE_IMAGE_SUPPORT" );
        keys.add( "CL_DEVICE_MAX_READ_IMAGE_ARGS" );
        keys.add( "CL_DEVICE_MAX_WRITE_IMAGE_ARGS" );
        keys.add( "CL_DEVICE_SINGLE_FP_CONFIG" );
        keys.add( "CL_DEVICE_IMAGE2D_MAX_WIDTH" );
        keys.add( "CL_DEVICE_IMAGE2D_MAX_HEIGHT" );
        keys.add( "CL_DEVICE_IMAGE3D_MAX_WIDTH" );
        keys.add( "CL_DEVICE_IMAGE3D_MAX_HEIGHT" );
        keys.add( "CL_DEVICE_IMAGE3D_MAX_DEPTH" );
        keys.add( "CL_DEVICE_PREFERRED_VECTOR_WIDTH_<t>" );
    	
        // devices
        ArrayList<Properties> devProps = new ArrayList<Properties>();
        for (int i=0; i<platforms.length; i++)
        {
            String platformName = OpenCLAlgorithmBase.getString(platforms[i], CL_PLATFORM_NAME);

            // Obtain the number of devices for the current platform
            int numDevices[] = new int[1];
            clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_ALL, 0, null, numDevices);

            System.out.println("Number of devices in platform "+platformName+": "+numDevices[0]);

            cl_device_id devicesArray[] = new cl_device_id[numDevices[0]];
            clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_ALL, numDevices[0], devicesArray, null);
            
            for ( int d = 0; d < devicesArray.length; d++ )
            {
            	cl_device_id device = devicesArray[d];

            	temp = new Properties();

            	// CL_DEVICE_NAME
            	temp.setProperty( "CL_DEVICE_NAME", OpenCLAlgorithmBase.getString(device, CL_DEVICE_NAME) );

            	// CL_DEVICE_VENDOR

            	// CL_DRIVER_VERSION

            	// CL_DEVICE_TYPE
            	long deviceType = OpenCLAlgorithmBase.getLong(device, CL_DEVICE_TYPE);
            	String tempString = new String();
            	if( (deviceType & CL_DEVICE_TYPE_CPU) != 0)
            		tempString = new String( "CL_DEVICE_TYPE_CPU");
            	if( (deviceType & CL_DEVICE_TYPE_GPU) != 0)
            		tempString = new String( "CL_DEVICE_TYPE_GPU");
            	if( (deviceType & CL_DEVICE_TYPE_ACCELERATOR) != 0)
            		tempString = new String( "CL_DEVICE_TYPE_ACCELERATOR");
            	if( (deviceType & CL_DEVICE_TYPE_DEFAULT) != 0)
            		tempString = new String( "CL_DEVICE_TYPE_DEFAULT");
            	temp.setProperty( "CL_DEVICE_TYPE", tempString );

            	// CL_DEVICE_MAX_COMPUTE_UNITS
            	temp.setProperty( "CL_DEVICE_MAX_COMPUTE_UNITS", String.valueOf( OpenCLAlgorithmBase.getInt(device, CL_DEVICE_MAX_COMPUTE_UNITS) ) );

            	// CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS
            	temp.setProperty( "CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS", String.valueOf( OpenCLAlgorithmBase.getLong(device, CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS) ) );

            	// CL_DEVICE_MAX_WORK_ITEM_SIZES
            	long maxWorkItemSizes[] = OpenCLAlgorithmBase.getLongs(device, CL_DEVICE_MAX_WORK_ITEM_SIZES, 3);
            	tempString = new String( String.valueOf(maxWorkItemSizes[0]) + " / " + String.valueOf(maxWorkItemSizes[1])
            			+ " / " + String.valueOf( maxWorkItemSizes[2] ));
            	temp.setProperty( "CL_DEVICE_MAX_WORK_ITEM_SIZES", tempString );

            	// CL_DEVICE_MAX_WORK_GROUP_SIZE
            	temp.setProperty( "CL_DEVICE_MAX_WORK_GROUP_SIZE", String.valueOf( OpenCLAlgorithmBase.getLong(device, CL_DEVICE_MAX_WORK_GROUP_SIZE) ) );

            	// CL_DEVICE_MAX_CLOCK_FREQUENCY
            	temp.setProperty( "CL_DEVICE_MAX_CLOCK_FREQUENCY", String.valueOf( OpenCLAlgorithmBase.getLong(device, CL_DEVICE_MAX_CLOCK_FREQUENCY) + " MHz" ) );

            	// CL_DEVICE_ADDRESS_BITS
            	temp.setProperty( "CL_DEVICE_ADDRESS_BITS", String.valueOf( OpenCLAlgorithmBase.getInt(device, CL_DEVICE_ADDRESS_BITS) ) );

            	// CL_DEVICE_MAX_MEM_ALLOC_SIZE
            	temp.setProperty( "CL_DEVICE_MAX_MEM_ALLOC_SIZE", String.valueOf( OpenCLAlgorithmBase.getLong(device, CL_DEVICE_MAX_MEM_ALLOC_SIZE) / (1024 * 1024) ) + " MByte" );

            	// CL_DEVICE_GLOBAL_MEM_SIZE
            	temp.setProperty( "CL_DEVICE_GLOBAL_MEM_SIZE", String.valueOf( OpenCLAlgorithmBase.getLong(device, CL_DEVICE_GLOBAL_MEM_SIZE) / (1024 * 1024) ) + " MByte" );

            	// CL_DEVICE_ERROR_CORRECTION_SUPPORT
            	temp.setProperty( "CL_DEVICE_ERROR_CORRECTION_SUPPORT", OpenCLAlgorithmBase.getInt(device, CL_DEVICE_ERROR_CORRECTION_SUPPORT) != 0 ? "yes" : "no" );

            	// CL_DEVICE_LOCAL_MEM_TYPE
            	temp.setProperty( "CL_DEVICE_LOCAL_MEM_TYPE", OpenCLAlgorithmBase.getInt(device, CL_DEVICE_LOCAL_MEM_TYPE) == 1 ? "local" : "global" );

            	// CL_DEVICE_LOCAL_MEM_SIZE
            	temp.setProperty( "CL_DEVICE_LOCAL_MEM_SIZE", String.valueOf( OpenCLAlgorithmBase.getLong(device, CL_DEVICE_LOCAL_MEM_SIZE) / 1024) + " KByte" );

            	// CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE
            	temp.setProperty( "CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE", String.valueOf( OpenCLAlgorithmBase.getLong(device, CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE) / 1024) + " KByte" );

            	// CL_DEVICE_QUEUE_PROPERTIES
            	tempString = new String();
            	long queueProperties = OpenCLAlgorithmBase.getLong(device, CL_DEVICE_QUEUE_PROPERTIES);
            	if(( queueProperties & CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE ) != 0) {
            		tempString = tempString + new String( "CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE");
                	if(( queueProperties & CL_QUEUE_PROFILING_ENABLE ) != 0)
                		tempString = tempString + new String( "\n");
            	}
            	if(( queueProperties & CL_QUEUE_PROFILING_ENABLE ) != 0)
            		tempString = tempString + new String( "CL_QUEUE_PROFILING_ENABLE");
            	temp.setProperty( "CL_DEVICE_QUEUE_PROPERTIES", tempString );

            	// CL_DEVICE_IMAGE_SUPPORT
            	temp.setProperty( "CL_DEVICE_IMAGE_SUPPORT", String.valueOf( OpenCLAlgorithmBase.getInt(device, CL_DEVICE_IMAGE_SUPPORT) ) );

            	// CL_DEVICE_MAX_READ_IMAGE_ARGS
            	temp.setProperty( "CL_DEVICE_MAX_READ_IMAGE_ARGS", String.valueOf( OpenCLAlgorithmBase.getInt(device, CL_DEVICE_MAX_READ_IMAGE_ARGS) ) );

            	// CL_DEVICE_MAX_WRITE_IMAGE_ARGS
            	temp.setProperty( "CL_DEVICE_MAX_WRITE_IMAGE_ARGS", String.valueOf( OpenCLAlgorithmBase.getInt(device, CL_DEVICE_MAX_WRITE_IMAGE_ARGS) ) );

            	// CL_DEVICE_SINGLE_FP_CONFIG
            	temp.setProperty( "CL_DEVICE_SINGLE_FP_CONFIG", stringFor_cl_device_fp_config( OpenCLAlgorithmBase.getLong(device, CL_DEVICE_SINGLE_FP_CONFIG) ) );

            	// CL_DEVICE_IMAGE2D_MAX_WIDTH
            	temp.setProperty( "CL_DEVICE_IMAGE2D_MAX_WIDTH", String.valueOf( OpenCLAlgorithmBase.getLong(device, CL_DEVICE_IMAGE2D_MAX_WIDTH) ) );

            	// CL_DEVICE_IMAGE2D_MAX_HEIGHT
            	temp.setProperty( "CL_DEVICE_IMAGE2D_MAX_HEIGHT", String.valueOf( OpenCLAlgorithmBase.getLong(device, CL_DEVICE_IMAGE2D_MAX_HEIGHT) ) );

            	// CL_DEVICE_IMAGE3D_MAX_WIDTH
            	temp.setProperty( "CL_DEVICE_IMAGE3D_MAX_WIDTH", String.valueOf( OpenCLAlgorithmBase.getLong(device, CL_DEVICE_IMAGE3D_MAX_WIDTH) ) );

            	// CL_DEVICE_IMAGE3D_MAX_HEIGHT
            	temp.setProperty( "CL_DEVICE_IMAGE3D_MAX_HEIGHT", String.valueOf( OpenCLAlgorithmBase.getLong(device, CL_DEVICE_IMAGE3D_MAX_HEIGHT) ) );

            	// CL_DEVICE_IMAGE3D_MAX_DEPTH
            	temp.setProperty( "CL_DEVICE_IMAGE3D_MAX_DEPTH", String.valueOf( OpenCLAlgorithmBase.getLong(device, CL_DEVICE_IMAGE3D_MAX_DEPTH) ) );

            	// CL_DEVICE_PREFERRED_VECTOR_WIDTH_<type>
            	int preferredVectorWidthChar = OpenCLAlgorithmBase.getInt(device, CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR);
            	int preferredVectorWidthShort = OpenCLAlgorithmBase.getInt(device, CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT);
            	int preferredVectorWidthInt = OpenCLAlgorithmBase.getInt(device, CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT);
            	int preferredVectorWidthLong = OpenCLAlgorithmBase.getInt(device, CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG);
            	int preferredVectorWidthFloat = OpenCLAlgorithmBase.getInt(device, CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT);
            	int preferredVectorWidthDouble = OpenCLAlgorithmBase.getInt(device, CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE);
            	tempString = new String(
            			"CHAR " + preferredVectorWidthChar + ", SHORT " + preferredVectorWidthShort 
            			+ ", INT " + preferredVectorWidthInt + ", LONG " +  preferredVectorWidthLong
            			+ ", FLOAT " + preferredVectorWidthFloat + ", DOUBLE " +  preferredVectorWidthDouble);
            	temp.setProperty( "CL_DEVICE_PREFERRED_VECTOR_WIDTH_<t>", tempString );
            	devProps.add(temp);
            }
        }
        fillTable(keys, devProps, html);

        html.append("</table>");

        return html.toString();
    }

    /**
     * Calculates the spans in order to fill in a StringBuilder table with a list of keys, properties.
     * @param keys property names
     * @param properties property values
     * @param sb table
     */
    private static void fillTable(List<String> keys, List<Properties> properties, StringBuilder sb) {
        ArrayList<Integer> spans = new ArrayList<Integer>(properties.size());
        for (int i = 0; i < properties.size(); i++) {
            spans.add(1);
        }
        fillTable(keys, properties, spans, sb);
    }

    /**
     * Fills in a StrinBuilder table with a list of keys, properties, and spans.
     * @param keys property names
     * @param properties property values
     * @param spans spans for spacing the table cells
     * @param sb table
     */
    private static void fillTable( List<String> keys, List<Properties> properties, List<Integer> spans, StringBuilder sb) {
        boolean header = true;
        for (String key : keys ) {
            sb.append("<tr>");
                cell(sb, key);
                int i = 0;
                for (Properties map : properties) {
                    cell(sb, spans.get(i), map.getProperty(key), header);
                    i++;
                }
            sb.append("</tr>");
            header = false;
        }
    }


    /**
     * Adds the property name to the StringBuilder table.
     * @param sb table
     * @param value property name
     */
    private static void cell(StringBuilder sb, String value) {
        sb.append("<td>").append(value).append("</td>");
    }

    /**
     * Adds the property values to the StringBuilder table, using the span for spacing.
     * @param sb table
     * @param span amount of spacing
     * @param value property value
     * @param header when true the cell is a header, so use a bold.
     */
    private static void cell(StringBuilder sb, int span, String value, boolean header) {
        if(header) {
            sb.append("<th colspan=\"").append(span).append("\">").append(value).append("</th>");
        }else{
            sb.append("<td colspan=\"").append(span).append("\">").append(value).append("</td>");
        }
    }
}