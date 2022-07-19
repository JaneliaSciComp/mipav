package gov.nih.mipav.view.renderer.J3D;


import javax.vecmath.*;


/**
 * The base class for lights in a general lighting system to calculate colors at a point in space. Each point has a
 * position, a normal vector, and material attributes that specify various colors, shininess, and an alpha value to
 * support blending of colors. Each light has three colors: an ambient color, a diffuse color, and a specular color. For
 * now, only the derived class MjAmbientLight affects the ambient lighting. Classes MjDirectionalLight, MjPointLight,
 * and MjSpotLight make no contribution to ambient light, only to diffuse and specular light. This can change if need
 * be. A light has an intensity. Although the intensity is intended to be in [0,1], it can be made larger to provide
 * oversaturation. Each light also has a Boolean variable indicating if the light is on or off. The derived light
 * classes include ambient, direction, point, and spot lights. The point and spot lights can have their effects
 * attenuated with distance. The spot light can have its effect attenuated based on angle from the axis of the spot
 * cone. The base class provides storage for the attenuation factors. Only a point or spot light should change these.
 *
 * <p>See ShearWarpRendering.pdf for a detailed description of the lighting model.</p>
 */

public abstract class SoftwareLight {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * The colors associated with the light, typically all white. The color channels are all in [0,1], hence the use of
     * Color3f.
     */
    public final Color3f ambient;

    /** DOCUMENT ME! */
    public final Color3f diffuse;

    /**
     * The intensity of the light, typically in [0,1]. The intensity may be chosen larger than 1 to create
     * oversaturation effects.
     */
    public float intensity;

    /** A flag indicating whether or not the light is on. */
    public boolean on;

    /** DOCUMENT ME! */
    public final Color3f specular;

    /**
     * Attenuation for point and spot lights based on distance of the vertex from the light source. The value is always
     * 1 for ambient and directional lights since those lights have no light position, in which case distance to a
     * vertex is undefined. See the document ShearWarpRendering.pdf for details about attenuation.
     */
    protected float m_fAttenuate;

    /**
     * Attenuation for spot lights based on angle between the unit-length axis of the spot cone and the vector from the
     * spot light position to the vertex. The value is always 1 for ambient, directional, and point lights. See the
     * document ShearWarpRendering.pdf for details about attenuation.
     */
    protected float m_fSpot;

    /**
     * The color m_kColor is the object returned in the colorOf functions. This object is a Point3f because the final
     * colors represents colors, each in the range [0,255]. Color3f specifies that color values are in [0,1], so I
     * avoided this type in case Java wants to throw exceptions when the values are outside [0,1]. Notice that the
     * material colors are Point3f in [0,255]^3 and the light colors are Color3f in [0,1]^3, so the products of material
     * and light colors are Point3f in [0,255]^3.
     */
    protected final Color3f m_kColor = new Color3f();

    /** DOCUMENT ME! */
    protected final Vector3f m_kNormal = new Vector3f();

    /** To avoid memory re allocations. The reflection vector is used in addSpecular. */
    protected final Vector3f m_kReflect = new Vector3f();

    /** Values updated in the initView method which must be called by all lights (except AmbientLight). */
    protected final Vector3f m_kView = new Vector3f();

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Construct a light. The default colors are all black, the intensity is zero, and the light is off. The attenuation
     * parameter are set to 1 for ambient and directional lights. Point and spot lights can modify the attenuation
     * parameters as desired.
     */
    protected SoftwareLight() {
        ambient = new Color3f(0.0f, 0.0f, 0.0f);
        diffuse = new Color3f(0.0f, 0.0f, 0.0f);
        specular = new Color3f(0.0f, 0.0f, 0.0f);
        intensity = 0.0f;
        on = false;

        // no attenuation
        m_fAttenuate = 1.0f;
        m_fSpot = 1.0f;
    }

    /**
     * Copy (deep) constructor.
     *
     * @param  kThat  Light What to make "this" light like.
     */
    protected SoftwareLight(SoftwareLight kThat) {
        ambient = new Color3f(kThat.ambient);
        diffuse = new Color3f(kThat.diffuse);
        specular = new Color3f(kThat.specular);
        intensity = kThat.intensity;
        on = kThat.on;
        m_fAttenuate = kThat.m_fAttenuate;
        m_fSpot = kThat.m_fSpot;

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
    public abstract Color3f colorOf(SoftwareMaterial kMaterial, SoftwareVertexProperty kVertexProperty, Point3f kEye);

    /**
     * Create a deep copy of this Light instance.
     *
     * @return  Light Deep copy of this Light instance.
     */
    public abstract SoftwareLight copy();

    /**
     * Calculate the color due to ambient lighting. See the document ShearWarpRendering.pdf for details about the
     * lighting model. This function is used in the colorOf functions in derived classes.
     *
     * @param  kMatAmbient  the material ambient color
     */
    protected final void addAmbient(Color3f kMatAmbient) {
        m_kColor.x = ambient.x * kMatAmbient.x;
        m_kColor.y = ambient.y * kMatAmbient.y;
        m_kColor.z = ambient.z * kMatAmbient.z;
    }

    /**
     * Calculate the color due to diffuse lighting. See the document ShearWarpRendering.pdf for details about the
     * lighting model. This function is used in the colorOf functions in derived classes.
     *
     * @param   kMaterial        SoftwareMaterial Default properties for the material including emissive, ambient,
     *                           diffuse, and specular colors along with the specular shininess which can be used to
     *                           disable specular highlighting.
     * @param   kVertexProperty  SoftwareVertexProperty Properties of the vertex which include its position and normal
     *                           vector along with optional material property overrides for the diffuse and specular
     *                           colors.
     * @param   kDirection       The unit-length direction of the light at the point. This direction depends on the type
     *                           of light.
     *
     * @return  The dot product of the direction and normal vector. If the dot product is nonnegative, there is no
     *          diffuse lighting at the point. Consequently, there is no specular lighting at the point. The caller of
     *          addDiffuse uses the returned dot product to test if addSpecular needs to be called.
     */
    protected final float addDiffuse(SoftwareMaterial kMaterial, SoftwareVertexProperty kVertexProperty,
                                     Vector3f kDirection) {

        Color3f kDiffuse = kVertexProperty.getDiffuse();

        if (null == kDiffuse) {
            kDiffuse = kMaterial.diffuse;
        }

        float fDdN = kDirection.dot(m_kNormal);

        if (fDdN < 0.0f) {
            float fDiffAmpl = -m_fAttenuate * m_fSpot * fDdN;
            m_kColor.x += fDiffAmpl * diffuse.x * kDiffuse.x;
            m_kColor.y += fDiffAmpl * diffuse.y * kDiffuse.y;
            m_kColor.z += fDiffAmpl * diffuse.z * kDiffuse.z;
        }

        return fDdN;
    }

    /**
     * Calculate the color due to specular lighting. See the document ShearWarpRendering.pdf for details about the
     * lighting model. This function is used in the colorOf functions in derived classes.
     *
     * @param  kMaterial        SoftwareMaterial Default properties for the material including emissive, ambient,
     *                          diffuse, and specular colors along with the specular shininess which can be used to
     *                          disable specular highlighting.
     * @param  kVertexProperty  SoftwareVertexProperty Properties of the vertex which include its position and normal
     *                          vector along with optional material property overrides for the diffuse and specular
     *                          colors.
     * @param  kDirection       The unit-length direction of the light at the point. This direction depends on the type
     *                          of light.
     * @param  fDdN             The dot product between the direction and normal. This is the return value of
     *                          addDiffuse.
     */
    protected final void addSpecular(SoftwareMaterial kMaterial, SoftwareVertexProperty kVertexProperty,
                                     Vector3f kDirection, float fDdN) {

        Color3f kSpecular = kVertexProperty.getSpecular();

        if (null == kSpecular) {
            kSpecular = kMaterial.specular;
        }

        m_kReflect.scaleAdd(-2.0f * fDdN, m_kNormal, kDirection);

        float fRdU = m_kReflect.dot(m_kView);

        if (fRdU < 0.0f) {
            fRdU = (float) Math.pow(-fRdU, kMaterial.shininess);

            float fSpecAmpl = m_fAttenuate * m_fSpot * fRdU; // Correct method ?

            // float fSpecAmpl = 2 * kMaterial.shininess * m_fAttenuate * m_fSpot * fRdU;
            m_kColor.x += fSpecAmpl * specular.x * kSpecular.x;
            m_kColor.y += fSpecAmpl * specular.y * kSpecular.y;
            m_kColor.z += fSpecAmpl * specular.z * kSpecular.z;
        }
    }

    /**
     * Calculate the view direction based on the vertex and the eye point (in model coordinates). Reverse the direction
     * of the normal vector if it is backfacing (relative to the view direction).
     *
     * @param  kEye             Point3f The location of the observer. Usually this is the camera location, but it is not
     *                          necessary.
     * @param  kVertexProperty  SoftwareVertexProperty Contains the position of the point to be lit and the unit-length
     *                          vector at the point.
     */
    protected final void initView(Point3f kEye, SoftwareVertexProperty kVertexProperty) {
        m_kView.sub(kVertexProperty.getPosition(), kEye);
        normalize(m_kView);

        // invert backfacing normals
        Vector3f kNormal = kVertexProperty.getNormal();

        if (m_kView.dot(kNormal) <= 0.0f) {
            m_kNormal.set(kNormal);
        } else {
            m_kNormal.negate(kNormal);
        }
    }

    /**
     * Replace a vector by a unit-length vector in the same direction. If the input vector is (nearly) the zero vector,
     * the replacement vector is the zero vector.
     *
     * @param   kV  the vector to be normalized
     *
     * @return  the length of the input vector
     */
    protected final float normalize(Vector3f kV) {
        float fLengthSquared = (kV.x * kV.x) + (kV.y * kV.y) + (kV.z * kV.z);

        if (0.0f < fLengthSquared) {
            float fLength = (float) Math.sqrt(fLengthSquared);
            kV.scale(1.0f / fLength);

            return fLength;
        } else {
            kV.set(0.0f, 0.0f, 0.0f);

            return 0.0f;
        }
    }
}
