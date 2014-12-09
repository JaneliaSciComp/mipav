package gov.nih.mipav.view.renderer.J3D;


import javax.vecmath.*;


/**
 * The class for directional lights in a general lighting system to calculate diffuse and specular colors at a point in
 * space.
 *
 * <p>See ShearWarpRendering.pdf for a detailed description of the lighting model.</p>
 */

public class SoftwareLightDirectional extends SoftwareLight {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** the light's direction in the world. */
    protected final Vector3f direction;

    /** DOCUMENT ME! */
    Color3f ambientColor = new Color3f();

    /** DOCUMENT ME! */
    Color3f diffuseColor = new Color3f();

    /** DOCUMENT ME! */
    Vector3f L = new Vector3f();

    /** DOCUMENT ME! */
    Vector3f N = new Vector3f();

    /** DOCUMENT ME! */
    Vector3f R = new Vector3f();

    /** DOCUMENT ME! */
    Color3f result = new Color3f();

    /** DOCUMENT ME! */
    Color3f specularColor = new Color3f();

    /** DOCUMENT ME! */
    Vector3f V = new Vector3f();

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Construct a directional light. The default colors are all black, the intensity is zero, and the light is off. The
     * attenuation parameters are set to 1 (no attenuation) since a directional light does not have a position that
     * identifies its source. The default direction is the zero vector.
     */
    public SoftwareLightDirectional() {
        direction = new Vector3f(0.0f, 0.0f, 0.0f);
    }

    /**
     * Copy (deep) constructor.
     *
     * @param  kThat  Light What to make "this" light like.
     */
    public SoftwareLightDirectional(SoftwareLightDirectional kThat) {
        super(kThat);
        direction = new Vector3f(kThat.direction);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Compute the contribution of color from this light on the specified material with specified vertex properties.
     *
     * @param   kMaterial        SoftwareMaterial Default properties for the material including emissive, ambient,
     *                           diffuse, and specular colors along with the specular shininess which can be used to
     *                           disable specular highlighting.
     * @param   kVertexProperty  SoftwareVertexProperty Properties of the vertex which include its position and normal
     *                           vector along with optional material property overrides for the diffuse and specular
     *                           colors.
     * @param   kEye             Point3f The location of the observer in the coordinate system defined for the light.
     *
     * @return  Color3f Reference to the computed color contribution by this light with this vertex and its material
     *          properties. The instance returned is owned by this light.
     */
    public final Color3f colorOf(SoftwareMaterial kMaterial, SoftwareVertexProperty kVertexProperty, Point3f kEye) {

        // no ambient light
        m_kColor.set(0.0f, 0.0f, 0.0f);
        result.set(0.0f, 0.0f, 0.0f);

        float cos_theta, cos_alpha;
        ;

        Color3f vertexDiffuse;

        V.sub(kEye, kVertexProperty.getPosition());
        V.normalize();

        N = kVertexProperty.getNormal();
        N.normalize();

        L.set(direction);
        L.negate();
        L.normalize();

        R.set(N.x, N.y, N.z);
        R.scale(2 * L.dot(N));
        R.sub(L);
        R.normalize();

        cos_theta = L.dot(N);
        cos_alpha = R.dot(V);

        vertexDiffuse = kVertexProperty.getDiffuse();

        if (cos_theta > 0) {
            diffuseColor.x = diffuse.x * vertexDiffuse.x;
            diffuseColor.y = diffuse.y * vertexDiffuse.y;
            diffuseColor.z = diffuse.z * vertexDiffuse.z;

            diffuseColor.scale(1.0f * cos_theta); // 1.0f for diffuse reflection factor
            result.add(diffuseColor);
        }

        if (cos_alpha > 0) {
            specularColor.set(specular);
            specularColor.scale(0.6f * (float) Math.pow(cos_alpha, kMaterial.shininess)); // 0.6f for specular
                                                                                          // reflection factor
            result.add(specularColor);
        }

        ambientColor.set(0.1f, 0.1f, 0.1f);
        ambientColor.scale(0.5f); // 0.5f for ambient reflection factor
        result.add(ambientColor);

        m_kColor.set(result);

        m_kColor.scale(intensity);

        // saturate at 1
        m_kColor.x = (m_kColor.x > 1) ? 1 : m_kColor.x;
        m_kColor.y = (m_kColor.y > 1) ? 1 : m_kColor.y;
        m_kColor.z = (m_kColor.z > 1) ? 1 : m_kColor.z;

        return m_kColor;
               /*
                * initView(kEye, kVertexProperty); m_kColor.set(0.0f, 0.0f, 0.0f); float fDdN = addDiffuse(kMaterial,
                * kVertexProperty, direction); if (fDdN < 0.0f && kMaterial.hasSpecular()) { addSpecular(kMaterial,
                * kVertexProperty, direction, fDdN); }
                *
                * m_kColor.scale(intensity); return m_kColor;
                */
    }

    /**
     * Create a deep copy of this Light instance.
     *
     * @return  Light Deep copy of this Light instance.
     */
    public SoftwareLight copy() {
        return new SoftwareLightDirectional(this);
    }

    /**
     * Get the light direction.
     *
     * @return  direction light direction
     */
    public final Vector3f getDirection() {
        return direction;
    }

    /**
     * Set the light direction.
     *
     * @param  v  light direction
     */
    public final void setDirection(Vector3f v) {
        direction.set(v);
        normalize(direction);
    }

    /**
     * Set the light direction.
     *
     * @param  fX  float X coordinate of the light direction vector.
     * @param  fY  float Y coordinate of the light direction vector.
     * @param  fZ  float Z coordinate of the light direction vector.
     */
    public final void setDirection(float fX, float fY, float fZ) {
        direction.set(fX, fY, fZ);
        normalize(direction);
    }
}
