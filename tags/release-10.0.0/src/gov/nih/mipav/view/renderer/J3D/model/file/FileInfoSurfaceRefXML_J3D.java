package gov.nih.mipav.view.renderer.J3D.model.file;


import gov.nih.mipav.model.file.*;
import javax.media.j3d.*;
import javax.vecmath.*;

/**
 * This structure contains the information that describes how an XML surface (see surface.xsd and FileSurfaceXML.java)
 * is stored on disk.
 *
 * @see  FileIO
 * @see  FileInfoXML
 * @see  FileSurfaceXML
 */
public class FileInfoSurfaceRefXML_J3D extends FileInfoSurfaceRefXML {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Material properties of the surface:. */
    private Material m_kMaterial = null;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Main constructor for FileInfoSurfaceXML.
     *
     * @param  name       String file name
     * @param  directory  String file directory
     * @param  format     int file format (data type)
     */
    public FileInfoSurfaceRefXML_J3D(String name, String directory, int format) {
        super(name, directory, format);
        m_kMaterial = new Material();
        m_kMaterial.setCapability(Material.ALLOW_COMPONENT_READ);
        m_kMaterial.setCapability(Material.ALLOW_COMPONENT_WRITE);
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
    public Material getMaterial() {
        return m_kMaterial;
    }
    

    /**
     * Sets the ambient color of the surface:
     *
     * @param  kColor  DOCUMENT ME!
     */
    public void setAmbient(Color3f kColor) {
        m_kMaterial.setAmbientColor(kColor);
    }
    
    /**
     * Sets the diffuse color of the surface:
     *
     * @param  kColor  DOCUMENT ME!
     */
    public void setDiffuse(Color3f kColor) {
        m_kMaterial.setDiffuseColor(kColor);
    }

    /**
     * Sets the emissive color of the surface:
     *
     * @param  kColor  DOCUMENT ME!
     */
    public void setEmissive(Color3f kColor) {
        m_kMaterial.setEmissiveColor(kColor);
    }

    /**
     * Sets the material properties for the surface:
     *
     * @param  kMaterial  DOCUMENT ME!
     */
    public void setMaterial(Material kMaterial) {
        m_kMaterial = kMaterial;
    }
    
    /**
     * Sets the surface shininess:
     *
     * @param  fShininess  DOCUMENT ME!
     */
    public void setShininess(float fShininess) {
        m_kMaterial.setShininess(fShininess);
    }


    /**
     * Sets the specular color of the surface:
     *
     * @param  kColor  DOCUMENT ME!
     */
    public void setSpecular(Color3f kColor) {
        m_kMaterial.setSpecularColor(kColor);
    }

    /**
     * Used to propogate all FileInfoSurfaceRefXML private variables to other FileInfosSurfaceRefXML.
     *
     * @param  fInfo  FileInfoSurfaceRefXML file info to be copied into
     */
    public void updateFileInfos(FileInfoXML fInfo) {

        if (this == fInfo) {
            return;
        }
        super.updateFileInfos(fInfo);
        ((FileInfoSurfaceRefXML_J3D) fInfo).setMaterial(this.getMaterial());
    }
}
