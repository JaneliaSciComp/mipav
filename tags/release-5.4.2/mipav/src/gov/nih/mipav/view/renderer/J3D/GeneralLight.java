package gov.nih.mipav.view.renderer.J3D;


import java.awt.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * This class maintains an instance of a SoftwareLight and a (Java3D) Light where these instances are of the same type:
 *
 * <p>- ambient (SoftwareLightAmbient, AmbientLight) - directional (SoftwareLightDirectional, DirectionalLight) - point
 * (SoftwareLightPoint, PointLight) - spot (SoftwareLightSpot, SpotLight)</p>
 *
 * <p>An instance of this class can switch between any of these four types in which case a new pair of SoftwareLight and
 * (Java) Light instances are now referenced. Any changes to light properties are set for both types of lights and any
 * properties which are set which do not apply to a light of a particular type are maintained so that when the light
 * type is switched, the properties can be applied to that light if applicable and any properties of the light being
 * switched from are automatically set for the new type of light.</p>
 *
 * <p>The following is a list of the properties which can be set:</p>
 *
 * <p>- enable (on/off) - color (RGB) - intensity (scales color by factor in range [0,1]) - position (normalized to
 * [-1,1] range) - direction (unit length) - target for direction (equal to position + direction) - type</p>
 *
 * <p>Note that setting the direction or target causes subsequent updates to position to change the target or direction,
 * or vice versa.</p>
 *
 * <p>When an instance is created, a bit mask is specified which identifies which of the possible types of lights this
 * instances can be switched to. The initial type of light is one of the types for which this instance is enabled
 * starting with ambient, then directional, then point, and then spot.</p>
 */
public class GeneralLight {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Ambient light constant. */
    private static final int TYPE_AMBIENT = 0;

    /** Spot light constant. */
    private static final int TYPE_SPOT = 1;

    /** Point light constant. */
    private static final int TYPE_POINT = 2;

    /** Directinal light constant. */
    private static final int TYPE_DIRECTIONAL = 3;

    /** Light constant types array. */
    private static final String[] sakTypeName = new String[] { "ambient", "spot", "point", "directional" };

    /** Bit mask used to identify light types in combination. */
    public static final int MASK_AMBIENT = 1 << TYPE_AMBIENT;

    /** Spot light mask. */
    public static final int MASK_SPOT = 1 << TYPE_SPOT;

    /** Point light mask. */
    public static final int MASK_POINT = 1 << TYPE_POINT;

    /** Directional light mask */
    public static final int MASK_DIRECTIONAL = 1 << TYPE_DIRECTIONAL;

    /** All light types mask */
    public static final int MASK_ALL = MASK_AMBIENT | MASK_SPOT | MASK_POINT | MASK_DIRECTIONAL;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** General properties which can be shared by all lights types. */
    private boolean m_bEnabled = false;

    /**
     * Properties shared by directional and spot lights. If this flag is true, then updates to position change the
     * direction based on the current target. Otherwise, updates to position change the target based on the current
     * direction. This flag is changed when the target or direction are explicitly set.
     */
    private boolean m_bFixTarget = false;

    /** Attenuation factor red channel. */
    private float m_fAttenuationC0 = 1.0f;

    /** Attenuation factor green channel. */
    private float m_fAttenuationC1 = 0.0f;

    /** Attenuation factor blue channel. */
    private float m_fAttenuationC2 = 0.0f;

    /** Light intensity scale factor. */
    private float m_fIntensity = 0.5f;

    /** Shinness factor, exponential power value for Phong type shading model. */
    private float m_fShininess = 5f;

    /** Maximum coordinate scale factor for real positions. */
    private final float m_fSizeMax;

    /** Scale factors for placing [-1,+1] normalized positions and targets into real world space with dimensions. */
    private final float m_fSizeX;

    /** float Y coordinate scale factor for real positions. */
    private final float m_fSizeY;

    /** float Y coordinate scale factor for real positions. */
    private final float m_fSizeZ;

    /** Properties for spot lights only. */
    private float m_fSpotAngle = (float) (Math.toRadians(45.0));

    /** Spot light exponential factor. */
    private float m_fSpotExponent = (float) (Math.log(0.1) / Math.log(Math.cos(m_fSpotAngle)));

    /** Type of light and limit for available light types which can be selected. */
    private int m_iType;

    /** Light type mask. */
    private final int m_iTypeMask;

    /** Description associated with this light. */
    private String m_kDescription = new String("");

    /** Light direction. */
    private Vector3f m_kDirection = new Vector3f(0.0f, 0.0f, 1.0f);

    /** Light color. */
    private Color m_kLightColor = new Color(1.0f, 1.0f, 1.0f);

    /** Properties shared by point and spot lights. */
    private Point3f m_kPosition = new Point3f(0.0f, 0.0f, 0.0f);

    /** Light shooting target position. */
    private Point3f m_kTarget = new Point3f(0.0f, 0.0f, 0.0f);

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor. Note that the scale factors may be negative.
     *
     * @param  iTypeMask  int Bit mask which limits the types of lights which can be selected for this instance.
     * @param  fSizeX     float X coordinate scale factor for real positions.
     * @param  fSizeY     float Y coordinate scale factor for real positions.
     * @param  fSizeZ     float Z coordinate scale factor for real positions. after normalizations.
     */
    public GeneralLight(int iTypeMask, float fSizeX, float fSizeY, float fSizeZ) {

        // Remember these dimensions as these will be used when creating
        // SoftwareLight and (Java3D) Light instances.  Note that the
        // scale factors may be negative, so for the "max" we are
        // interested in the largest magnitude.
        m_fSizeX = fSizeX;
        m_fSizeY = fSizeY;
        m_fSizeZ = fSizeZ;
        m_fSizeMax = Math.max(Math.abs(m_fSizeX), Math.max(Math.abs(m_fSizeY), Math.abs(m_fSizeZ)));

        // remember this mask for the allowable types of this light
        m_iTypeMask = iTypeMask;

        // a light type must be selected before calling any other
        // the methods in this class
        if (canSetTypeAmbient()) {
            setTypeAmbient();
        } else if (canSetTypeDirectional()) {
            setTypeDirectional();
        } else if (canSetTypePoint()) {
            setTypePoint();
        } else if (canSetTypeSpot()) {
            setTypeSpot();
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Query if light can be set to the ambient type.
     *
     * @return  boolean True if light type can be set to ambient.
     */
    public final boolean canSetTypeAmbient() {
        return 0 != (m_iTypeMask & MASK_AMBIENT);
    }

    /**
     * Query if light can be set to the directional type.
     *
     * @return  boolean True if light type can be set to directional.
     */
    public final boolean canSetTypeDirectional() {
        return 0 != (m_iTypeMask & MASK_DIRECTIONAL);
    }

    /**
     * Query if light can be set to any non-ambient type.
     *
     * @return  boolean True if light type can be set to any non-ambient type.
     */
    public final boolean canSetTypeNonAmbient() {
        return 0 != (m_iTypeMask & ~MASK_AMBIENT);
    }

    /**
     * Query if light can be set to the point type.
     *
     * @return  boolean True if light type can be set to point.
     */
    public final boolean canSetTypePoint() {
        return 0 != (m_iTypeMask & MASK_POINT);
    }

    /**
     * Query if light can be set to the spot type.
     *
     * @return  boolean True if light type can be set to spot.
     */
    public final boolean canSetTypeSpot() {
        return 0 != (m_iTypeMask & MASK_SPOT);
    }

    /**
     * Access Java3D light instance based on current type.
     *
     * @return  Light Java3D light instance based on current type.
     */
    public Light createJava3dLight() {

        // Apply the scale factors to the position and direction.
        Point3f kPosition = createJava3dLightPosition();
        Vector3f kDirection = createJava3dLightDirection();

        // Create the instance of the light and set any properties
        // which are specific to that type of light.
        Light kLight;

        switch (m_iType) {

            default:
            case TYPE_AMBIENT:
                kLight = new AmbientLight();
                break;

            case TYPE_DIRECTIONAL:
                kLight = new DirectionalLight();
                ((DirectionalLight) kLight).setDirection(kDirection);
                break;

            case TYPE_POINT:
                kLight = new PointLight();
                ((PointLight) kLight).setPosition(kPosition);
                ((PointLight) kLight).setAttenuation(m_fAttenuationC0, m_fAttenuationC1, m_fAttenuationC2);
                break;

            case TYPE_SPOT:
                kLight = new SpotLight();
                ((SpotLight) kLight).setPosition(kPosition);
                ((SpotLight) kLight).setDirection(kDirection);
                ((SpotLight) kLight).setAttenuation(m_fAttenuationC0, m_fAttenuationC1, m_fAttenuationC2);
                ((SpotLight) kLight).setSpreadAngle(m_fSpotAngle);
                ((SpotLight) kLight).setConcentration(m_fSpotExponent);
                break;
        }

        // Set properties which are common to all lights.
        kLight.setEnable(m_bEnabled);

        Color3f kColor = new Color3f(m_kLightColor);
        kColor.scale(m_fIntensity);
        kLight.setColor(kColor);

        return kLight;
    }

    /**
     * Access Java3D light instance based on current type.
     *
     * @return  Light Java3D light instance based on current type.
     */
    public WildMagic.LibGraphics.Rendering.Light createWMLight() {

        // Apply the scale factors to the position and direction.
        Point3f kPosition = createJava3dLightPosition();
        Vector3f kDirection = createJava3dLightDirection();

        // Create the instance of the light and set any properties
        // which are specific to that type of light.
        WildMagic.LibGraphics.Rendering.Light kLight = null;

        switch (m_iType) {

            default:
            case TYPE_AMBIENT:
                kLight = new WildMagic.LibGraphics.Rendering.
                    Light(WildMagic.LibGraphics.Rendering.Light.LightType.LT_AMBIENT);
                break;

            case TYPE_DIRECTIONAL:
                kLight = new WildMagic.LibGraphics.Rendering.
                    Light(WildMagic.LibGraphics.Rendering.Light.LightType.LT_DIRECTIONAL);
                break;

            case TYPE_POINT:
                kLight = new WildMagic.LibGraphics.Rendering.
                    Light(WildMagic.LibGraphics.Rendering.Light.LightType.LT_POINT);
                kLight.Position = new WildMagic.LibFoundation.Mathematics.
                    Vector3f(kPosition.x, kPosition.y, kPosition.z);
                kLight.Constant = m_fAttenuationC0;
                kLight.Linear = m_fAttenuationC1;
                kLight.Quadratic = m_fAttenuationC2;
                break;

            case TYPE_SPOT:
                kLight = new WildMagic.LibGraphics.Rendering.
                    Light(WildMagic.LibGraphics.Rendering.Light.LightType.LT_SPOT);
                kLight.Position = new WildMagic.LibFoundation.Mathematics.
                    Vector3f(kPosition.x, kPosition.y, kPosition.z);
                
                kLight.Exponent = m_fSpotExponent;
                kLight.SetAngle(m_fSpotAngle);
                kLight.Constant = m_fAttenuationC0;
                kLight.Linear = m_fAttenuationC1;
                kLight.Quadratic = m_fAttenuationC2;
                break;
        }

        kLight.DVector = new WildMagic.LibFoundation.Mathematics.
            Vector3f(kDirection.x, kDirection.y, -kDirection.z);
        kLight.Ambient = new WildMagic.LibFoundation.Mathematics.
            ColorRGB( m_kLightColor.getRed()/255.0f, m_kLightColor.getGreen()/255.0f, m_kLightColor.getBlue()/255.0f );
        kLight.Diffuse = new WildMagic.LibFoundation.Mathematics.
            ColorRGB( m_kLightColor.getRed()/255.0f, m_kLightColor.getGreen()/255.0f, m_kLightColor.getBlue()/255.0f );
        kLight.Specular = new WildMagic.LibFoundation.Mathematics.
            ColorRGB( m_kLightColor.getRed()/255.0f, m_kLightColor.getGreen()/255.0f, m_kLightColor.getBlue()/255.0f );
        kLight.Intensity = m_fIntensity;

        return kLight;
    }

    /**
     * Create an instance which contains the normalized real space coordinates of the direction vector of the (Java3D)
     * Light.
     *
     * @return  Vector3f New instance created with direction vector.
     */
    public final Vector3f createJava3dLightDirection() {
        Vector3f kDirection = new Vector3f(m_kDirection);
        kDirection.x *= (m_fSizeX / m_fSizeMax);
        kDirection.y *= (m_fSizeY / m_fSizeMax);
        kDirection.z *= (m_fSizeZ / m_fSizeMax);
        normalize(kDirection);

        return kDirection;
    }

    /**
     * Create an instance which contains the normalized real space coordinates for the position of the (Java3D) Light.
     *
     * @return  Point3f New instance created with the position.
     */
    public final Point3f createJava3dLightPosition() {
        Point3f kPosition = new Point3f(m_kPosition);
        kPosition.x *= (m_fSizeX / m_fSizeMax);
        kPosition.y *= (m_fSizeY / m_fSizeMax);
        kPosition.z *= (m_fSizeZ / m_fSizeMax);

        return kPosition;
    }

    /**
     * Access current software light based on type. Coordinates and direction are appropriate for software rendering for
     * a light located in the "model" space of the objects being rendered.
     *
     * @return  SoftwareLight Software light instance based on current type.
     */
    public final SoftwareLight createSoftwareLightModel() {

        // Note that Y, Z (and not X) need to be negated.

        Point3f kPosition = new Point3f((+m_kPosition.x) * m_fSizeX, (-m_kPosition.y) * m_fSizeY,
                                        (-m_kPosition.z) * m_fSizeZ);

        Vector3f kDirection = new Vector3f((+m_kDirection.x) * m_fSizeX, (-m_kDirection.y) * m_fSizeY,
                                           (-m_kDirection.z) * m_fSizeZ);
        kDirection.normalize();

        return createSoftwareLight(kPosition, kDirection);
    }

    /**
     * Access current software light based on type. Coordinates and direction are appropriate for software rendering for
     * a light located in the "world" relative to objects being rendered.
     *
     * @return  SoftwareLight Software light instance based on current type.
     */
    public final SoftwareLight createSoftwareLightWorld() {

        // Note that X, Y, Z need to be negated.

        Point3f kPosition = new Point3f((-m_kPosition.x) * m_fSizeX, (-m_kPosition.y) * m_fSizeY,
                                        (-m_kPosition.z) * m_fSizeZ);

        Vector3f kDirection = new Vector3f((-m_kDirection.x) * m_fSizeX, (-m_kDirection.y) * m_fSizeY,
                                           (-m_kDirection.z) * m_fSizeZ);
        kDirection.normalize();

        return createSoftwareLight(kPosition, kDirection);
    }

    /**
     * Set whether light is enabled.
     *
     * @param  bEnable  boolean True if light is to be enabled.
     */
    public final void enable(boolean bEnable) {
        m_bEnabled = bEnable;
    }

    /**
     * Query the current based color of the light.
     *
     * @return  Color Base color of the light.
     */
    public final Color getColor() {
        return m_kLightColor;
    }

    /**
     * Get description associated with this light.
     *
     * @return  String Text description for light.
     */
    public final String getDescription() {
        return m_kDescription;
    }

    /**
     * Query the current direction of the light.
     *
     * @return  Vector3f Normalized direction vector for the light.
     */
    public final Vector3f getDirection() {
        return m_kDirection;
    }

    /**
     * Get the light color scaled with the intensity factor.
     *
     * @return  Light color!
     */
    public final Color3f getIntensifiedColor() {
        Color3f kColor = new Color3f(m_kLightColor);
        kColor.scale(m_fIntensity);

        return kColor;
    }

    /**
     * Query the current light intensity.
     *
     * @return  float Light color scale factor in range [0,1].
     */
    public final float getIntensity() {
        return m_fIntensity;
    }

    /**
     * Query the current position of the light.
     *
     * @return  Point3f Position of the light where the coordinates are normalized to the [-1,+1] range.
     */
    public final Point3f getPosition() {
        return m_kPosition;
    }

    /**
     * Query the current target of the light.
     *
     * @return  Point3f Target position of the light where the coordinates are normalized to the [-1,+1] range.
     */
    public final Point3f getTarget() {
        return m_kTarget;
    }

    /**
     * Return a string which describes the type of light.
     *
     * @return  String
     */
    public final String getTypeString() {
        return sakTypeName[m_iType];
    }

    /**
     * Query if updates to direction affect the target while keeping the direction fixed.
     *
     * @return  boolean True if the direction is to remain fixed.
     */
    public final boolean isDirectionFixed() {
        return !m_bFixTarget;
    }

    /**
     * Query if light is currently enabled.
     *
     * @return  boolean True if light is currently enabled.
     */
    public final boolean isEnabled() {
        return m_bEnabled;
    }

    /**
     * Query if updates to position affect direction while keeping the target fixed.
     *
     * @return  boolean True if the target is to remain fixed.
     */
    public final boolean isTargetFixed() {
        return m_bFixTarget;
    }

    /**
     * Query if current light type is ambient.
     *
     * @return  boolean True if current light type is ambient.
     */
    public boolean isTypeAmbient() {
        return TYPE_AMBIENT == m_iType;
    }

    /**
     * Query if current light type is directional.
     *
     * @return  boolean True if current light type is directional.
     */
    public final boolean isTypeDirectional() {
        return TYPE_DIRECTIONAL == m_iType;
    }

    /**
     * Query if current light type is point.
     *
     * @return  boolean True if current light type is point.
     */
    public final boolean isTypePoint() {
        return TYPE_POINT == m_iType;
    }

    /**
     * Query if current light type is spot.
     *
     * @return  boolean True if current light type is spot.
     */
    public final boolean isTypeSpot() {
        return TYPE_SPOT == m_iType;
    }

    /**
     * Set the base color for the light. Actual color of light is the base color multiplied by the light intensity.
     *
     * @param  kColor  Color Base color for the light.
     */
    public final void setColor(Color kColor) {
        m_kLightColor = kColor;
    }

    /**
     * Set description associated with this light.
     *
     * @param  description  String Text description for light.
     */
    public final void setDescription(String description) {
        m_kDescription = description;
    }

    /**
     * Set the current direction of the light.
     *
     * @param  kDir  Vector3f Normalized direction vector for the light.
     */
    public final void setDirection(Vector3f kDir) {
        setDirection(kDir.x, kDir.y, kDir.z);
    }

    /**
     * Set the current direction of the light.
     *
     * @param  fX  float X coordinate of light direction vector.
     * @param  fY  float Y coordinate of light direction vector.
     * @param  fZ  float Z coordinate of light direction vector.
     */
    public final void setDirection(float fX, float fY, float fZ) {

        updateDirection(fX, fY, fZ);
        m_bFixTarget = false;
        setPosition(m_kPosition);
    }

    /**
     * Set the intensity of the light which sets the actual color of the light to this factor times the base color of
     * the light.
     *
     * @param  fIntensity  float Normalized light color scale factor in range [0,1].
     */
    public final void setIntensity(float fIntensity) {
        m_fIntensity = fIntensity;
    }

    /**
     * Set the current position of the light.
     *
     * @param  kPosition  Point3f Position of light where the coordinates are normalized to the [-1,+1] range.
     */
    public final void setPosition(Point3f kPosition) {
        setPosition(kPosition.x, kPosition.y, kPosition.z);
    }

    /**
     * Set the current position of the light.
     *
     * @param  fX  float X coordinate position of light in [-1,+1] range.
     * @param  fY  float Y coordinate position of light in [-1,+1] range.
     * @param  fZ  float Z coordinate position of light in [-1,+1] range.
     */
    public final void setPosition(float fX, float fY, float fZ) {
        m_kPosition.set(fX, fY, fZ);

        if (m_bFixTarget) {
            updateDirection(m_kTarget.x - m_kPosition.x, m_kTarget.y - m_kPosition.y, m_kTarget.z - m_kPosition.z);
        } else {
            updateTarget(m_kPosition.x + m_kDirection.x, m_kPosition.y + m_kDirection.y,
                         m_kPosition.z + m_kDirection.z);
        }

    }

    /**
     * Set the current target of the light.
     *
     * @param  kPosition  Point3f Target position of light where the coordinates are normalized to the [-1,+1] range.
     */
    public final void setTarget(Point3f kPosition) {
        setTarget(kPosition.x, kPosition.y, kPosition.z);
    }

    /**
     * Set the current target of the light.
     *
     * @param  fX  float X coordinate target position of light in [-1,+1] range.
     * @param  fY  float Y coordinate target position of light in [-1,+1] range.
     * @param  fZ  float Z coordinate target position of light in [-1,+1] range.
     */
    public final void setTarget(float fX, float fY, float fZ) {
        updateTarget(fX, fY, fZ);
        m_bFixTarget = true;
        setPosition(m_kPosition);
    }

    /**
     * Set the current light type to ambient.
     */
    public final void setTypeAmbient() {

        if (canSetTypeAmbient()) {
            m_iType = TYPE_AMBIENT;
        }
    }

    /**
     * Set the current light type to directional.
     */
    public final void setTypeDirectional() {

        if (canSetTypeDirectional()) {
            m_iType = TYPE_DIRECTIONAL;
        }
    }

    /**
     * Set the current light type to point.
     */
    public final void setTypePoint() {

        if (canSetTypePoint()) {
            m_iType = TYPE_POINT;
        }
    }

    /**
     * Set the current light type to spot.
     */
    public final void setTypeSpot() {

        if (canSetTypeSpot()) {
            m_iType = TYPE_SPOT;
        }
    }

    /**
     * Access software light instance based on current type.
     *
     * @param   kPosition   Point3f Input position of light.
     * @param   kDirection  Vector3f Input (normalized) direction vector of light.
     *
     * @return  SoftwareLight Software light instance based on current type.
     */
    private SoftwareLight createSoftwareLight(Point3f kPosition, Vector3f kDirection) {

        // Create the instance of the light and set any properties
        // which are specific to that type of light.
        SoftwareLight kLight;

        switch (m_iType) {

            default:
            case TYPE_AMBIENT:
                kLight = new SoftwareLightAmbient();
                break;

            case TYPE_DIRECTIONAL:
                kLight = new SoftwareLightDirectional();
                ((SoftwareLightDirectional) kLight).setDirection(kDirection);
                break;

            case TYPE_POINT:
                kLight = new SoftwareLightPoint();
                ((SoftwareLightPoint) kLight).setPosition(kPosition);
                ((SoftwareLightPoint) kLight).setAttenuation(m_fAttenuationC0, m_fAttenuationC1, m_fAttenuationC2);
                break;

            case TYPE_SPOT:
                kLight = new SoftwareLightSpot();
                ((SoftwareLightSpot) kLight).setPosition(kPosition);
                ((SoftwareLightSpot) kLight).setDirection(kDirection);
                ((SoftwareLightSpot) kLight).setAttenuation(m_fAttenuationC0, m_fAttenuationC1, m_fAttenuationC2);
                ((SoftwareLightSpot) kLight).setAngle(m_fSpotAngle);
                ((SoftwareLightSpot) kLight).setExponent(m_fSpotExponent);
                break;
        }

        // Set properties which are common to all lights.
        kLight.on = m_bEnabled;
        kLight.intensity = m_fIntensity;
        kLight.ambient.set(m_kLightColor);
        kLight.diffuse.set(m_kLightColor);
        kLight.specular.set(m_kLightColor);

        return kLight;
    }

    /**
     * Replace a vector by a unit-length vector in the same direction. If the input vector is (nearly) the zero vector,
     * the replacement vector is the zero vector.
     *
     * @param   kV  the vector to be normalized
     *
     * @return  the length of the input vector
     */
    private float normalize(Vector3f kV) {
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

    /**
     * Set the current direction of the light.
     *
     * @param  fX  float X coordinate of light direction vector.
     * @param  fY  float Y coordinate of light direction vector.
     * @param  fZ  float Z coordinate of light direction vector.
     */
    private void updateDirection(float fX, float fY, float fZ) {

        m_kDirection.set(fX, fY, fZ);
        normalize(m_kDirection);
    }

    /**
     * Set the current target of the light.
     *
     * @param  fX  float X coordinate target position of light in [-1,+1] range.
     * @param  fY  float Y coordinate target position of light in [-1,+1] range.
     * @param  fZ  float Z coordinate target position of light in [-1,+1] range.
     */
    private void updateTarget(float fX, float fY, float fZ) {
        m_kTarget.set(fX, fY, fZ);
    }
}
