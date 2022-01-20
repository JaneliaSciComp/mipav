package gov.nih.mipav.util;



import java.net.URL;

import gov.nih.mipav.view.icons.PlaceHolder;
import WildMagic.LibGraphics.Shaders.ImageCatalog;


public class MipavInitGPU {
    public static boolean IsInit = false;
    public static void InitGPU()
    {
        if ( !IsInit )
        {
            IsInit = true;                   
            String kExternalDirs = getExternalDirs();        
            ImageCatalog.SetActive( new ImageCatalog("Main", kExternalDirs) );      
        }
    }   
    public static void RemoveGPU()
    {
        if ( IsInit )
        {     
            ImageCatalog.GetActive().dispose();        
            IsInit = false;                   
        }
    }



    /**
     * Get the default Shader directory.
     * @return string containing the default Shader directory.
     */
    static public String getExternalDirs()
    {
    	String dir = new String(System.getProperty("user.dir"));
    	dir = dir + System.getProperty("file.separator") + "src" + System.getProperty("file.separator") + "WildMagic";
    	return dir;
    }
}
