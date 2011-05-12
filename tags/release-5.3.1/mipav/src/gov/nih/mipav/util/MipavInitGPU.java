package gov.nih.mipav.util;



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
        String jar_filename = "";
        String class_path_key = "java.class.path";
        String class_path = System.getProperty(class_path_key);
        for (String fn : class_path.split(":")) {
            if (fn.contains("WildMagic.jar")) {
                jar_filename = fn;
                String externalDirs = jar_filename.substring(0, jar_filename.indexOf("lib"));
                externalDirs = externalDirs.concat("WildMagic");
                //System.err.println("Shader dir found: " + externalDirs);
                return externalDirs;
            }
        }
        System.err.println("Shader dir not found");
        return System.getProperties().getProperty("user.dir");
    }
}
