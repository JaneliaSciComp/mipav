package gov.nih.mipav.view.renderer.J3D.model.file;


import gov.nih.mipav.view.renderer.J3D.model.structures.*;
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
public class FileInfoSurfaceXML_J3D extends FileInfoSurfaceXML {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Material properties of the surface:. */
    private Material m_kMaterial = null;

    /** Surface triangle mesh:. */
    private ModelTriangleMesh[] m_kMesh = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Main constructor for FileInfoSurfaceXML.
     *
     * @param  name       String file name
     * @param  directory  String file directory
     * @param  format     int file format (data type)
     */
    public FileInfoSurfaceXML_J3D(String name, String directory, int format) {
        super(name, directory, format);
        m_kMaterial = new Material();
        m_kMaterial.setCapability(Material.ALLOW_COMPONENT_READ);
        m_kMaterial.setCapability(Material.ALLOW_COMPONENT_WRITE);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------
    /*
     * Prepares the class for cleanup.
     */
    public void finalize() {
        m_kMaterial = null;
        m_kMesh = null;
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
     * Returns the ModelTriangleMesh representing the surface:
     *
     * @return  DOCUMENT ME!
     */
    public ModelTriangleMesh[] getMesh() {
        return m_kMesh;
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
     * Sets the ModelTriangleMesh representing the surface:
     *
     * @param  kMesh  DOCUMENT ME!
     */
    public void setMesh(ModelTriangleMesh[] kMesh) {
        m_kMesh = kMesh;
    }

    /**
     * Creates the ModelTriangleMesh for the surface:
     *
     * @param  kVertices       Mesh coordinates
     * @param  kNormals        Mesh normals (may be null)
     * @param  kColors         Mesh colors (may be null)
     * @param  aiConnectivity  Mesh index connectivity array
     */
    public void setMesh(Point3f[] kVertices, Vector3f[] kNormals, Color4f[] kColors, int[] aiConnectivity) {

       int i;
       if ( m_kMesh == null ) {
         m_kMesh = new ModelTriangleMesh[1];
       } else {
         ModelTriangleMesh[] mesh = new ModelTriangleMesh[m_kMesh.length];
         for (i = 0; i < m_kMesh.length; i++) {
           mesh[i] = m_kMesh[i];
         }
         m_kMesh = new ModelTriangleMesh[meshIndex + 1];
         for (i = 0; i < meshIndex; i++) {
           m_kMesh[i] = mesh[i];
         }
       }
       m_kMesh[meshIndex++] = new ModelTriangleMesh(kVertices, kNormals, kColors, aiConnectivity);
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
     * Used to propogate all FileInfoSurfaceXML private variables to other FileInfosSurfaceXML.
     *
     * @param  fInfo  FileInfoSurfaceXML file info to be copied into
     */
    public void updateFileInfos(FileInfoXML fInfo) {

        if (this == fInfo) {
            return;
        }
        super.updateFileInfos(fInfo);
        ((FileInfoSurfaceXML_J3D) fInfo).setMaterial(this.getMaterial());
        ((FileInfoSurfaceXML_J3D) fInfo).setMesh(this.getMesh());
    }
}
