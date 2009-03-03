package gov.nih.mipav.view.renderer.J3D;


import javax.vecmath.*;


/**
 * The class for spot lights in a general lighting system to calculate diffuse and specular colors at a point in space.
 *
 * <p>See ShearWarpRendering.pdf for a detailed description of the lighting model.</p>
 */

public class SoftwareLightSpot extends SoftwareLightPoint {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** the spot cone angle and exponent. */
    protected float m_fAngle;

    /** cosine of the spot cone angle, to avoid computing cosine repeatedly. */
    protected float m_fCosAngle;

    /** DOCUMENT ME! */
    protected float m_fExponent;

    /** the spot cone unit-length axis. */
    protected final Vector3f m_kAxis;

    /** DOCUMENT ME! */
    Color3f ambientColor = new Color3f();

    /** DOCUMENT ME! */
    Color3f diffsum = new Color3f();

    /** DOCUMENT ME! */
    Vector3f L = new Vector3f();

    /** DOCUMENT ME! */
    Vector3f lightDirection = new Vector3f();

    /** DOCUMENT ME! */
    Vector3f Lneg = new Vector3f();

    /** DOCUMENT ME! */
    Vector3f N = new Vector3f();

    /** DOCUMENT ME! */
    Vector3f nScale = new Vector3f();

    /** DOCUMENT ME! */
    Color3f result = new Color3f();

    /** DOCUMENT ME! */
    Color3f specsum = new Color3f();

    /** DOCUMENT ME! */
    Vector3f specularVector = new Vector3f();

    /** DOCUMENT ME! */
    Vector3f V = new Vector3f();

    /** DOCUMENT ME! */
    Color3f vertexDiffuse;

    /** DOCUMENT ME! */
    Color3f vertexSpecular;

    /** DOCUMENT ME! */
    Vector3f viewDirection = new Vector3f();

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Construct a spot light. The default colors are all black, the intensity is zero, and the light is off. The
     * attenuation parameters are initialized to 1 (no attenuation). The class provides accessor methods for
     * attenuation.
     */
    public SoftwareLightSpot() {
        m_kAxis = new Vector3f(0.0f, 0.0f, 0.0f);
        setAngle((float) Math.PI);
        setExponent(1.0f);
    }

    /**
     * Copy (deep) constructor.
     *
     * @param  kThat  Light What to make "this" light like.
     */
    public SoftwareLightSpot(SoftwareLightSpot kThat) {
        super(kThat);
        m_kAxis = new Vector3f(kThat.m_kAxis);
        setAngle(kThat.getAngle());
        setExponent(kThat.getExponent());
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

        float cos_theta, cos_alpha;
        float fov_factor;
        float spec_factor;

        initView(kEye, kVertexProperty);

        m_kColor.set(0.0f, 0.0f, 0.0f);
        result.set(0.0f, 0.0f, 0.0f);
        specsum.set(0.0f, 0.0f, 0.0f);
        diffsum.set(0.0f, 0.0f, 0.0f);

        lightDirection = getDirection();

        viewDirection.set(m_kView);

        N = kVertexProperty.getNormal();

        fov_factor = lightDirection.dot(viewDirection);

        vertexDiffuse = kVertexProperty.getDiffuse();
        vertexSpecular = kVertexProperty.getSpecular();

        if ((fov_factor <= 0.0) || (fov_factor < m_fCosAngle)) {
            V.negate();
            fov_factor = 0.0f;
        } else {
            fov_factor = (((float) Math.cos((1.0 - fov_factor) * Math.PI / (1.0 - m_fCosAngle)) * 0.5f) + 0.5f);
            V.negate();
        }

        vertexDiffuse = kVertexProperty.getDiffuse();

        if (fov_factor > 0.0001f) {
            cos_theta = lightDirection.dot(N);

            if (cos_theta > 0) {
                diffsum.x = diffuse.x * cos_theta * fov_factor;
                diffsum.y = diffuse.y * cos_theta * fov_factor;
                diffsum.z = diffuse.z * cos_theta * fov_factor;
            }

            cos_theta *= 1.3f;
            Lneg.negate(L);
            nScale.scale(cos_theta, N);
            specularVector.add(Lneg, nScale);
            cos_alpha = specularVector.dot(viewDirection);

            if (cos_alpha > 0) {
                spec_factor = fov_factor * (float) Math.exp(Math.log(cos_alpha));
                specsum.x = diffuse.x * spec_factor;
                specsum.y = diffuse.y * spec_factor;
                specsum.z = diffuse.z * spec_factor;
            }
        }

        diffsum.x = (diffsum.x > 1) ? 1 : diffsum.x;
        diffsum.y = (diffsum.y > 1) ? 1 : diffsum.y;
        diffsum.z = (diffsum.z > 1) ? 1 : diffsum.z;
        specsum.x = (specsum.x > 1) ? 1 : specsum.x;
        specsum.y = (specsum.y > 1) ? 1 : specsum.y;
        specsum.z = (specsum.z > 1) ? 1 : specsum.z;

        ambientColor.set(0.1f, 0.1f, 0.1f);
        ambientColor.scale(0.5f); // 0.5f for ambient reflection factor

        result.x = (vertexDiffuse.x * (ambientColor.x + (vertexDiffuse.x * diffsum.x))) +
                   (vertexSpecular.x * specsum.x);
        result.y = (vertexDiffuse.y * (ambientColor.y + (vertexDiffuse.y * diffsum.y))) +
                   (vertexSpecular.y * specsum.y);
        result.z = (vertexDiffuse.z * (ambientColor.z + (vertexDiffuse.z * diffsum.z))) +
                   (vertexSpecular.z * specsum.z);

        // saturate at 1
        result.x = (result.x > 1) ? 1 : result.x;
        result.y = (result.y > 1) ? 1 : result.y;
        result.z = (result.z > 1) ? 1 : result.z;

        m_kColor.set(result);
        m_kColor.scale(intensity);

        return m_kColor;

        /*
         *       initView(kEye, kVertexProperty);
         *
         *    // no ambient light      m_kColor.set(0.0f, 0.0f, 0.0f);
         *
         *    // Compute V-P for vertex V and light position P.  The normalize      // function exists to make the
         * direction unit-length, but we happen      // to need the actual length for attenuation.  (Java3D's
         * normalize()      // does not return the length of the input.)
         * m_kDirection.sub(kVertexProperty.getPosition(), position);      float fDistance = normalize(m_kDirection);
         *   if (m_fExponent > 0.0f) computeSpot(m_kDirection);
         *
         *    // attenuate based on the spot cone      if (m_fSpot > 0.0f) { // attenuate based on the distance from
         * light to vertex if (m_bAttenuate)     computeAttenuation(fDistance);
         *
         * float fDdN = addDiffuse(kMaterial, kVertexProperty, m_kDirection); if (fDdN < 0.0f && kMaterial.hasSpecular())
         * {     addSpecular(kMaterial, kVertexProperty, m_kDirection, fDdN); }      }
         *
         *    m_kColor.scale(intensity);      return m_kColor;
         */
    }

    /**
     * Create a deep copy of this Light instance.
     *
     * @return  Light Deep copy of this Light instance.
     */
    public SoftwareLight copy() {
        return new SoftwareLightSpot(this);
    }

    /**
     * Get the spot cone angle.
     *
     * @return  the spot cone angle
     */
    public float getAngle() {
        return m_fAngle;
    }

    /**
     * Get the light direction.
     *
     * @return  m_kDirection light direction
     */
    public Vector3f getDirection() {
        return m_kAxis;
    }

    /**
     * Get the spot cone exponent.
     *
     * @return  the spot cone exponent
     */
    public float getExponent() {
        return m_fExponent;
    }

    /**
     * Set the spot cone angle. The angle must be in [0,pi).
     *
     * @param  fAngle  the spot cone angle
     */
    public void setAngle(float fAngle) {
        m_fAngle = fAngle;
        m_fCosAngle = (float) Math.cos(m_fAngle);
    }

    /**
     * Set the light direction.
     *
     * @param  v  light direction
     */
    public void setDirection(Vector3f v) {
        m_kAxis.set(v);
        normalize(m_kAxis);
    }

    /**
     * Set the light direction.
     *
     * @param  fX  float X coordinate of the light direction vector.
     * @param  fY  float Y coordinate of the light direction vector.
     * @param  fZ  float Z coordinate of the light direction vector.
     */
    public void setDirection(float fX, float fY, float fZ) {
        m_kAxis.set(fX, fY, fZ);
        normalize(m_kAxis);
    }

    /**
     * Set the spot cone exponent. The exponent must be nonnegative. If it is zero, the light is completely attenuated.
     *
     * @param  fExponent  the spot cone exponent
     */
    public void setExponent(float fExponent) {
        m_fExponent = fExponent;
        m_fSpot = 1.0f;
    }

    /**
     * Compute the attenuation due to the spot cone geometry.
     *
     * @param  kDirection  the unit-length direction from the light's position to the vertex
     */
    protected void computeSpot(Vector3f kDirection) {
        float fDdA = kDirection.dot(m_kAxis);

        if (fDdA >= m_fCosAngle) {
            m_fSpot = (float) Math.pow(fDdA, m_fExponent);
        } else {
            m_fSpot = 0.0f;
        }
    }
}
