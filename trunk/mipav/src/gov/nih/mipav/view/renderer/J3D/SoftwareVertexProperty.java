package gov.nih.mipav.view.renderer.J3D;


import javax.vecmath.*;


/**
 * This class maintains the following properties for a vertex.
 *
 * <p>- position (required) - normal (required) - diffuse (optional) - specular (optional)</p>
 *
 * <p>The optional properties are specified by setting their references to null. In this case, the value will be
 * determined from the SoftwareMaterial reference that is specified.</p>
 */
public class SoftwareVertexProperty {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private Color3f m_kDiffuse = null;

    /** DOCUMENT ME! */
    private Vector3f m_kNormal = new Vector3f(0.0f, 0.0f, 1.0f);

    /** By default, vertex diffuse and specular colors are optional. */
    private Point3f m_kPosition = new Point3f(0.0f, 0.0f, 0.0f);

    /** DOCUMENT ME! */
    private Color3f m_kSpecular = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor. Default vertex properties.
     */
    public SoftwareVertexProperty() { }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Enable/disable the optional diffuse color vertex property. Must call this method to enable the property before
     * calling the setDiffuse method to change its value. If enabling the property, the default color is set to white.
     *
     * @param  bEnable  boolean True to enable; false to disable.
     */
    public final void enableDiffuse(boolean bEnable) {
        m_kDiffuse = bEnable ? (new Color3f(1.0f, 1.0f, 1.0f)) : null;
    }

    /**
     * Enable/disable the optional specular color vertex property. Must call this method to enable the property before
     * calling the setSpecular method to change its value. If enabling the property, the default color is set to white.
     *
     * @param  bEnable  boolean True to enable; false to disable.
     */
    public final void enableSpecular(boolean bEnable) {
        m_kSpecular = bEnable ? (new Color3f(1.0f, 1.0f, 1.0f)) : null;
    }

    /**
     * Query current optional diffuse color for vertex. Can also be used to determine if this property is enabled.
     *
     * @return  Color3f Non-null reference to vertex diffuse color if it is defined.
     */
    public final Color3f getDiffuse() {
        return m_kDiffuse;
    }

    /**
     * Query current normal vector for vertex.
     *
     * @return  Vector3f Non-null reference to vertex normal.
     */
    public final Vector3f getNormal() {
        return m_kNormal;
    }

    /**
     * Query current position of vertex.
     *
     * @return  Point3f Non-null reference to vertex position.
     */
    public final Point3f getPosition() {
        return m_kPosition;
    }

    /**
     * Query current optional specular color for vertex. Can also be used to determine if this property is enabled.
     *
     * @return  Color3f Non-null reference to vertex specular color if it is defined.
     */
    public final Color3f getSpecular() {
        return m_kSpecular;
    }

    /**
     * Query if the vertex diffuse color property is enabled.
     *
     * @return  boolean True if enabled.
     */
    public final boolean isEnabledDiffuse() {
        return null != m_kDiffuse;
    }

    /**
     * Query if the vertex specular color property is enabled.
     *
     * @return  boolean True if enabled.
     */
    public final boolean isEnabledSpecular() {
        return null != m_kSpecular;
    }

    /**
     * Set the diffuse color for the vertex. The vertex diffuse color property must be enabled.
     *
     * @param  kDiffuse  Color3f Diffuse color for the vertex.
     */
    public final void setDiffuse(Color3f kDiffuse) {
        m_kDiffuse.set(kDiffuse);
    }

    /**
     * Set the diffuse color for the vertex.
     *
     * @param  fR  float Red channel of vertex diffuse color.
     * @param  fG  float Green channel of vertex diffuse color.
     * @param  fB  float Blue channel of vertex diffuse color.
     */
    public final void setDiffuse(float fR, float fG, float fB) {
        m_kDiffuse.set(fR, fG, fB);
    }

    /**
     * Set the normal vector for the vertex.
     *
     * @param  kNormal  Vector3f Normal vector for the vertex.
     */
    public final void setNormal(Vector3f kNormal) {
        m_kNormal.set(kNormal);
    }

    /**
     * Set the normal vector for the vertex.
     *
     * @param  fX  float X coordinate of vertex normal vector.
     * @param  fY  float Y coordinate of vertex normal vector.
     * @param  fZ  float Z coordinate of vertex normal vector.
     */
    public final void setNormal(float fX, float fY, float fZ) {
        m_kNormal.set(fX, fY, fZ);
    }

    /**
     * Set the position of the vertex.
     *
     * @param  kPosition  Point3f Position coordinates for the vertex.
     */
    public final void setPosition(Point3f kPosition) {
        m_kPosition.set(kPosition);
    }

    /**
     * Set the position of the vertex.
     *
     * @param  fX  float X coordinate of vertex position.
     * @param  fY  float Y coordinate of vertex position.
     * @param  fZ  float Z coordinate of vertex position.
     */
    public final void setPosition(float fX, float fY, float fZ) {
        m_kPosition.set(fX, fY, fZ);
    }

    /**
     * Set the specular color for the vertex. The vertex specular color property must be enabled.
     *
     * @param  kSpecular  Color3f Specular color for the vertex.
     */
    public final void setSpecular(Color3f kSpecular) {
        m_kSpecular.set(kSpecular);
    }

    /**
     * Set the specular color for the vertex.
     *
     * @param  fR  float Red channel of vertex specular color.
     * @param  fG  float Green channel of vertex specular color.
     * @param  fB  float Blue channel of vertex specular color.
     */
    public final void setSpecular(float fR, float fG, float fB) {
        m_kSpecular.set(fR, fG, fB);
    }
}
