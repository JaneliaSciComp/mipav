package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;


import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.Rendering.*;

/**
 * This structure contains the information that describes how an XML surface (see surface.xsd and FileSurfaceXML.java)
 * is stored on disk.
 *
 * @see  FileIO
 * @see  FileInfoXML
 * @see  FileSurfaceXML
 */
public class FileInfoSurfaceRefXML_WM extends FileInfoSurfaceRefXML {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Material properties of the surface:. */
    private MaterialState m_kMaterial = null;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Main constructor for FileInfoSurfaceXML.
     *
     * @param  name       String file name
     * @param  directory  String file directory
     * @param  format     int file format (data type)
     */
    public FileInfoSurfaceRefXML_WM(String name, String directory, int format) {
        super(name, directory, format);
        m_kMaterial = new MaterialState();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares the class for cleanup.
     */
    public void finalize() {
        m_kMaterial = null;
        super.finalize();
    }
    
   
    /**
     * Returns the material properties for the surface:
     *
     * @return  DOCUMENT ME!
     */
    public MaterialState getMaterial() {
        return m_kMaterial;
    }
    

    /**
     * Sets the ambient color of the surface:
     *
     * @param  kColor  DOCUMENT ME!
     */
    public void setAmbient(ColorRGB kColor) {
        m_kMaterial.Ambient.Copy(kColor);
    }
    
    /**
     * Sets the diffuse color of the surface:
     *
     * @param  kColor  DOCUMENT ME!
     */
    public void setDiffuse(ColorRGB kColor) {
        m_kMaterial.Diffuse.Copy(kColor);
    }

    /**
     * Sets the emissive color of the surface:
     *
     * @param  kColor  DOCUMENT ME!
     */
    public void setEmissive(ColorRGB kColor) {
        m_kMaterial.Emissive.Copy(kColor);
    }

    /**
     * Sets the specular color of the surface:
     *
     * @param  kColor  DOCUMENT ME!
     */
    public void setSpecular(ColorRGB kColor) {
        m_kMaterial.Specular.Copy(kColor);
    }


    /**
     * Sets the surface shininess:
     *
     * @param  fShininess  DOCUMENT ME!
     */
    public void setShininess(float fShininess) {
        m_kMaterial.Shininess = fShininess;
    }

    /**
     * Sets the material properties for the surface:
     *
     * @param  kMaterial  DOCUMENT ME!
     */
    public void setMaterial(MaterialState kMaterial) {
        m_kMaterial = kMaterial;
    }
    

    /**
     * Used to propogate all FileInfoSurfaceRefXML private variables to other FileInfosSurfaceRefXML.
     *
     * @param  fInfo  FileInfoSurfaceRefXML file info to be copied into
     */
    public void updateFileInfos(FileInfoXML fInfo) {
        if (this == fInfo)
        {
            return;
        }
        super.updateFileInfos(fInfo);
        ((FileInfoSurfaceRefXML_WM) fInfo).setMaterial(this.getMaterial());
    }
}
