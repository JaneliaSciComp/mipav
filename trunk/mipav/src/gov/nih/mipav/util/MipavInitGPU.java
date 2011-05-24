package gov.nih.mipav.util;



import java.net.URL;

import gov.nih.mipav.view.icons.PlaceHolder;
import WildMagic.LibGraphics.Shaders.CompiledProgramCatalog;
import WildMagic.LibGraphics.Shaders.ImageCatalog;
import WildMagic.LibGraphics.Shaders.PixelProgramCatalog;
import WildMagic.LibGraphics.Shaders.VertexProgramCatalog;


public class MipavInitGPU {
    public static boolean IsInit = false;
    public static void InitGPU()
    {
        if ( !IsInit )
        {
            IsInit = true;                   
            String kExternalDirs = getExternalDirs();        
            ImageCatalog.SetActive( new ImageCatalog("Main", kExternalDirs) );      
            VertexProgramCatalog.SetActive(new VertexProgramCatalog("Main", kExternalDirs));       
            PixelProgramCatalog.SetActive(new PixelProgramCatalog("Main", kExternalDirs));
            CompiledProgramCatalog.SetActive(new CompiledProgramCatalog());
        }
    }   
    public static void RemoveGPU()
    {
        if ( IsInit )
        {     
            ImageCatalog.GetActive().dispose();      
            VertexProgramCatalog.GetActive().dispose();      
            PixelProgramCatalog.GetActive().dispose();      
            CompiledProgramCatalog.GetActive().dispose();      
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
