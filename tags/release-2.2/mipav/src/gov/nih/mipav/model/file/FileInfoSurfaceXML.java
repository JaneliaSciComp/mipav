package gov.nih.mipav.model.file;


import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.structures.*;

import java.util.Hashtable;
import java.util.Enumeration;
import java.util.Vector;
import java.util.StringTokenizer;
import java.io.Serializable;

import javax.media.j3d.*;
import javax.vecmath.*;


/**
 * This structure contains the information that describes how an XML surface
 * (see surface.xsd and FileSurfaceXML.java) is stored on disk.
 *
 * @see     FileIO
 * @see     FileInfoXML
 * @see     FileSurfaceXML
 */
public class FileInfoSurfaceXML extends FileInfoXML {

    /* Unique ID, for the surface: */
    private int m_iUnique_ID;
    /* Material properties of the surface: */
    private Material m_kMaterial = null;
    /* Type keyword for the surface: */
    private String m_kType = null;
    /* Surface triangle mesh: */
    private ModelTriangleMesh m_kMesh = null;

    /**
     * Main constructor for FileInfoSurfaceXML
     * @param name String file name
     * @param directory String file directory
     * @param format int file format (data type)
     */
    public FileInfoSurfaceXML( String name, String directory, int format )
    {
        super( name, directory, format );
        m_kMaterial = new Material();
        m_kMaterial.setCapability( Material.ALLOW_COMPONENT_READ );
        m_kMaterial.setCapability( Material.ALLOW_COMPONENT_WRITE );
    }

    /**
     * Prepares the class for cleanup
     */
    public void finalize()
    {
        m_kMaterial = null;
        m_kType = null;
        m_kMesh = null;

        super.finalize();
    }

    /* Set the unique id for the surface: */
    public void setID( int iID )
    {
        m_iUnique_ID = iID;
    }

    /* Returns the unique id for the surface: */
    public int getID( )
    {
        return m_iUnique_ID;
    }

    /* set the surface type keyword: */
    public void setType( String kType )
    {
        m_kType = kType;
    }

    /* Returns the surface type keyword: */
    public String getType( )
    {
        return m_kType;
    }

    /* Sets the material properties for the surface: */
    public void setMaterial( Material kMaterial )
    {
        m_kMaterial = kMaterial;
    }

    /* Returns the material properties for the surface: */
    public Material getMaterial()
    {
        return m_kMaterial;
    }

    /* Sets the ModelTriangleMesh representing the surface: */
    public void setMesh( ModelTriangleMesh kMesh )
    {
        m_kMesh = kMesh;
    }

    /* Returns the ModelTriangleMesh representing the surface: */
    public ModelTriangleMesh getMesh()
    {
        return m_kMesh;
    }

    /* Sets the ambient color of the surface: */
    public void setAmbient( Color3f kColor )
    {
        m_kMaterial.setAmbientColor( kColor );
    }
    
    /* Sets the diffuse color of the surface: */
    public void setDiffuse( Color3f kColor )
    {
        m_kMaterial.setDiffuseColor( kColor );
    }

    /* Sets the emissive color of the surface: */
    public void setEmissive( Color3f kColor )
    {
        m_kMaterial.setEmissiveColor( kColor );
    }

    /* Sets the specular color of the surface: */
    public void setSpecular( Color3f kColor )
    {
        m_kMaterial.setSpecularColor( kColor );
    }

    /* Sets the surface shininess: */
    public void setShininess( float fShininess )
    {
        m_kMaterial.setShininess( fShininess );           
    }
    
    /* Creates the ModelTriangleMesh for the surface: */
    public void setMesh( Point3f[] kVertices,
                         Vector3f[] kNormals,
                         int[] aiConnectivity )
    {
        if ( kNormals != null )
        {
            m_kMesh = new ModelTriangleMesh( kVertices, kNormals, aiConnectivity );
        }
        else
        {
            m_kMesh = new ModelTriangleMesh( kVertices, aiConnectivity );
        }
    }

    /**
     *  Displays the file information
     *  @param dlog    JDialogBase dialog box that is written to
     */
    public void displayAboutInfo( JDialogBase dlog, TransMatrix kMat) {}


    /**
     *   Used to propogate all FileInfoSurfaceXML private variables to other
     *   FileInfosSurfaceXML
     *   @param fInfo FileInfoSurfaceXML file info to be copied into
     */
    public void updateFileInfos( FileInfoXML fInfo )
    {
        if ( this == fInfo ) {
            return;
        }

        fInfo.setImageDescription( this.getImageDescription() );
        ((FileInfoSurfaceXML)fInfo).setID( this.getID() );
        ((FileInfoSurfaceXML)fInfo).setMaterial( this.getMaterial() );
        ((FileInfoSurfaceXML)fInfo).setType( this.getType() );
        ((FileInfoSurfaceXML)fInfo).setMesh( this.getMesh() );
    }
}
