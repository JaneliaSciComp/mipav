package gov.nih.mipav.view.renderer.J3D;


import javax.vecmath.*;


/**
 * The class for point lights in a general lighting system to calculate diffuse and specular colors at a point in space.
 *
 * <p>See ShearWarpRendering.pdf for a detailed description of the lighting model.</p>
 */

public class SoftwareLightPoint extends SoftwareLight {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected boolean m_bAttenuate;

    /**
     * Attenuation coefficients (inverse quadratic attenuation). The Boolean flag is 'false' whenever (a0,a1,a2) =
     * (1,0,0), in which case the inverse quadratic attenuation is the identity mapping and no execution time should be
     * spent evaluating the attenuation factor.
     */
    protected float m_fA0, m_fA1, m_fA2;

    /**
     * To avoid memory allocations. The vector is used to store the difference between a vertex to be lit and the
     * light's position.
     */
    protected final Vector3f m_kDirection = new Vector3f();

    /** the light's position. */
    protected final Point3f position;

    /** DOCUMENT ME! */
    Color3f ambientColor = new Color3f();

    /** DOCUMENT ME! */
    float cos_theta, cos_alpha;

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

    /** DOCUMENT ME! */
    Color3f vertexDiffuse;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Construct a point light. The default colors are all black, the intensity is zero, and the light is off. The
     * attenuation parameters are initialized to 1 (no attenuation). The light is positioned by default at the origin.
     */
    public SoftwareLightPoint() {
        position = new Point3f(0.0f, 0.0f, 0.0f);

        // attenuation coefficients (no attenuation is the default)
        m_fA0 = 1.0f;
        m_fA1 = 0.0f;
        m_fA2 = 0.0f;
        m_bAttenuate = false;
    }

    /**
     * Copy (deep) constructor.
     *
     * @param  kThat  Light What to make "this" light like.
     */
    public SoftwareLightPoint(SoftwareLightPoint kThat) {
        super(kThat);
        position = new Point3f(kThat.position);
        m_fA0 = kThat.m_fA0;
        m_fA1 = kThat.m_fA1;
        m_fA2 = kThat.m_fA2;
        m_bAttenuate = kThat.m_bAttenuate;
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
    public Color3f colorOf(SoftwareMaterial kMaterial, SoftwareVertexProperty kVertexProperty, Point3f kEye) {

        m_kColor.set(0.0f, 0.0f, 0.0f);

        initView(kEye, kVertexProperty);
        result.set(0.0f, 0.0f, 0.0f);
        V.set(m_kView);
        V.negate();
        V.normalize();

        N = kVertexProperty.getNormal();
        N.normalize();

        L.sub(position, kVertexProperty.getPosition());

        // L = getDirection();
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
                * initView(kEye, kVertexProperty);
                *
                * // no ambient light m_kColor.set(0.0f, 0.0f, 0.0f);
                *
                * // Compute V-P for vertex V and light position P.  The normalize // function exists to make the direction
                * unit-length, but we happen // to need the actual length for attenuation.  (Java3D's normalize() // does not
                * return the length of the input.) m_kDirection.sub(kVertexProperty.getPosition(), position); float fDistance =
                * normalize(m_kDirection); if (m_bAttenuate) computeAttenuation(fDistance);
                *
                * float fDdN = addDiffuse(kMaterial, kVertexProperty, m_kDirection); if (fDdN < 0.0f && kMaterial.hasSpecular())
                * { addSpecular(kMaterial, kVertexProperty, m_kDirection, fDdN); }
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
        return new SoftwareLightPoint(this);
    }

    /**
     * Get the constant term of the attenuation polynomial.
     *
     * @return  coefficient a0
     */
    public float getAttenuation0() {
        return m_fA0;
    }

    /**
     * Get the linear term of the attenuation polynomial.
     *
     * @return  coefficient a1
     */
    public float getAttenuation1() {
        return m_fA1;
    }

    /**
     * Get the quadratic term of the attenuation polynomial.
     *
     * @return  coefficient a2
     */
    public float getAttenuation2() {
        return m_fA1;
    }

    /**
     * Get the light source position.
     *
     * @return  position light source position
     */
    public Point3f getPosition() {
        return position;
    }

    /**
     * Set the coefficients for inverse quadratic attenuation, attenuation = 1/(a0+a1*d+a2*d^2) where d is the distance
     * from the light's position to the vertex. The coefficients must satisfy a0 >= 0, a1 >= 0, and a2 >= 0, but not all
     * can be zero. No attenuation occurs when a0 = 1, a1 = 0, and a2 = 0.
     *
     * @param  fA0  coefficient a0
     * @param  fA1  coefficient a1
     * @param  fA2  coefficient a2
     */
    public void setAttenuation(float fA0, float fA1, float fA2) {
        m_fA0 = fA0;
        m_fA1 = fA1;
        m_fA2 = fA2;
        m_bAttenuate = (fA0 != 1.0f) || (fA1 != 0.0f) || (fA2 != 0.0f);
    }

    /**
     * Set the light source position.
     *
     * @param  p  source position
     */
    public void setPosition(Point3f p) {
        position.set(p);
    }

    /**
     * Set the light source position.
     *
     * @param  fX  float X coordinate of light source position.
     * @param  fY  float Y coordinate of light source position.
     * @param  fZ  float Z coordinate of light source position.
     */
    public void setPosition(float fX, float fY, float fZ) {
        position.set(fX, fY, fZ);
    }

    /**
     * Compute the attenuation factor as an inverse quadratic polynomial.
     *
     * @param  fDistance  the distance from the light's position to the vertex to be lit
     */
    protected void computeAttenuation(float fDistance) {
        m_fAttenuate = 1.0f / (m_fA0 + (fDistance * (m_fA1 + (fDistance * m_fA2))));
    }
}
