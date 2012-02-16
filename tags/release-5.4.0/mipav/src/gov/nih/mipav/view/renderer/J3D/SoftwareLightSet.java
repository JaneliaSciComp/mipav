package gov.nih.mipav.view.renderer.J3D;


import javax.vecmath.*;


/**
 * This class maintains a set of SoftwareLight instances where each light can be of type ambient, directional, point, or
 * spot. Furthermore, each light can have position and direction in model coordinates or in world coordinates where a
 * transformation can be specified which converts world coordinates to model coordinates. This class is also used to
 * apply the complete set of lighting to a "cell" in model coordinates which consists of a set of coordinates, a normal
 * vector, and a color along with some other default "material" properties.
 */
public class SoftwareLightSet {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Array of software lights in model coordinates. */
    private SoftwareLight[] m_akLightsModel = new SoftwareLight[0];

    /**
     * Array of world coordinate vectors for software world lights. An array item may be null if that software light
     * does not have a direction (e.g., ambient or point).
     */
    private Vector3f[] m_akLightsWorldDirection = new Vector3f[0];

    /**
     * Array of model coordinate vectors for software world lights corresponding to the entries in the
     * m_akLightsWorldDirection array.
     */
    private Vector3f[] m_akLightsWorldDirectionInModelSpace = new Vector3f[0];

    /** Array of software lights converted from world coordinates to model coordinates. */
    private SoftwareLight[] m_akLightsWorldInModelSpace = new SoftwareLight[0];

    /**
     * Array of world coordinate positions for software world lights. An array item may be null if that software light
     * does not have a position (e.g., ambient or directional).
     */
    private Point3f[] m_akLightsWorldPosition = new Point3f[0];

    /**
     * Array of model coordinate positions for software world lights corresponding to the entries in the
     * m_akLightsWorldPosition array.
     */
    private Point3f[] m_akLightsWorldPositionInModelSpace = new Point3f[0];

    /** Avoid memory reallocations. */
    private Vector3f m_kV = new Vector3f();

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor. Creates empty set of lights.
     */
    public SoftwareLightSet() { }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Apply a 3x3 transformation to the software world light position and direction which are in world coordinates so
     * that they are converted to model coordinates.
     *
     * @param  kTransform  Matrix3f Contains the 3x3 rotation transformation.
     */
    public void applyWorldToModelTransform(Matrix3f kTransform) {

        // convert each world space light to model space coordinates
        for (int i = 0; i < m_akLightsWorldPosition.length; i++) {
            Point3f kPosition = m_akLightsWorldPosition[i];

            if (null != kPosition) {
                m_kV.set(kPosition);
                kTransform.transform(kPosition, m_akLightsWorldPositionInModelSpace[i]);
            }
        }

        for (int i = 0; i < m_akLightsWorldDirection.length; i++) {
            Vector3f kDirection = m_akLightsWorldDirection[i];

            if (null != kDirection) {
                m_kV.set(kDirection);
                kTransform.transform(kDirection, m_akLightsWorldDirectionInModelSpace[i]);
            }
        }
    }

    /**
     * Apply a 3x3 transformation to the software world light position and direction which are in world coordinates so
     * that they are converted to model coordinates.
     *
     * @param  kAxisX  Vector3f X axis vector of 3x3 transformation.
     * @param  kAxisY  Vector3f Y axis vector of 3x3 transformation.
     * @param  kAxisZ  Vector3f Z axis vector of 3x3 transformation.
     */
    public void applyWorldToModelTransform(Vector3f kAxisX, Vector3f kAxisY, Vector3f kAxisZ) {

        // convert each world space light to model space coordinates
        for (int i = 0; i < m_akLightsWorldPosition.length; i++) {
            Point3f kPosition = m_akLightsWorldPosition[i];

            if (null != kPosition) {
                m_kV.set(kPosition);
                m_akLightsWorldPositionInModelSpace[i].set(kAxisX.dot(m_kV), kAxisY.dot(m_kV), kAxisZ.dot(m_kV));
            }
        }

        for (int i = 0; i < m_akLightsWorldDirection.length; i++) {
            Vector3f kDirection = m_akLightsWorldDirection[i];

            if (null != kDirection) {
                m_kV.set(kDirection);
                m_akLightsWorldDirectionInModelSpace[i].set(kAxisX.dot(m_kV), kAxisY.dot(m_kV), kAxisZ.dot(m_kV));
            }
        }
    }

    /**
     * Compute the "lighted" color of the cell at the specified position with the specified normal vector and base
     * color. This is based on application of all the world and model lights to this cell.
     *
     * @param   kMaterial        SoftwareMaterial Contains default material properties for lighting some of which may be
     *                           overridden by properties specified in the vertex propertyies.
     * @param   kVertexProperty  SoftwareVertexProperty Contains the position, normal, and optional diffuse/specular
     *                           colors defined for this cell.
     * @param   kEyeModel        Point3f Position of the eyepoint in model coordinates.
     *
     * @return  Color3f Computed "lighted" color clamped so as not to exceed white.
     */
    public Color3f getCellColor(SoftwareMaterial kMaterial, SoftwareVertexProperty kVertexProperty, Point3f kEyeModel) {
    	Color3f m_kColor = new Color3f();
        
        // accumulate color from each light
        m_kColor.set(kMaterial.emissive);

        // apply model lights
        for (int iLight = 0; iLight < m_akLightsModel.length; iLight++) {
            SoftwareLight kL = m_akLightsModel[iLight];

            if (kL.on) {
                m_kColor.add(kL.colorOf(kMaterial, kVertexProperty, kEyeModel));
            }
        }

        // apply world lights
        for (int iLight = 0; iLight < m_akLightsWorldInModelSpace.length; iLight++) {
            SoftwareLight kL = m_akLightsWorldInModelSpace[iLight];

            if (kL.on) {
                m_kColor.add(kL.colorOf(kMaterial, kVertexProperty, kEyeModel));
            }
        }

        m_kColor.clamp(0.0f, 1.0f);

        return m_kColor;
    }

    /**
     * Determine if any "model" relative lights are enabled.
     *
     * @return  boolean True if any "model" relative lights are enabled.
     */
    public boolean isAnyModelLightEnabled() {

        for (int i = 0; i < m_akLightsModel.length; i++) {

            if (m_akLightsModel[i].on) {
                return true;
            }
        }

        return false;
    }

    /**
     * Determine if any "world" positioned lights are enabled.
     *
     * @return  boolean True if any "world" positioned lights are enabled.
     */
    public boolean isAnyWorldLightEnabled() {

        for (int i = 0; i < m_akLightsWorldInModelSpace.length; i++) {

            if (m_akLightsWorldInModelSpace[i].on) {
                return true;
            }
        }

        return false;
    }

    /**
     * Specify a set of software lights defined in model coordinates.
     *
     * @param  akLights  SoftwareLight[] Array of software lights in model coordinats.
     */
    public void setModelLights(SoftwareLight[] akLights) {

        // Make a copy of the model based lights.
        // Need to negate the Y and Z coordinates.
        m_akLightsModel = new SoftwareLight[akLights.length];

        for (int i = 0; i < akLights.length; i++) {
            m_akLightsModel[i] = akLights[i].copy();
        }
    }

    /**
     * Specify a set of software lights defined in world coordinates.
     *
     * <p>Note: If a transformation was applied previously to convert world lights to model coordinates, this
     * transformation will have to be respecified to convert these world lights to model coordinates.</p>
     *
     * @param  akLights  SoftwareLight[] Array of software lights in world coordinates.
     */
    public void setWorldLights(SoftwareLight[] akLights) {

        // All position and vector coordinates in world space need
        // to be negated.

        // duplicate the array of world lights because they will have
        // to be transformed to model space for rendering
        m_akLightsWorldInModelSpace = new SoftwareLight[akLights.length];
        m_akLightsWorldPosition = new Point3f[akLights.length];
        m_akLightsWorldPositionInModelSpace = new Point3f[akLights.length];
        m_akLightsWorldDirection = new Vector3f[akLights.length];
        m_akLightsWorldDirectionInModelSpace = new Vector3f[akLights.length];

        for (int i = 0; i < akLights.length; i++) {
            m_akLightsWorldInModelSpace[i] = akLights[i].copy();

            // Spot light - copy position and direction
            if (akLights[i] instanceof SoftwareLightSpot) {
                SoftwareLightSpot kLightWorld = (SoftwareLightSpot) akLights[i];
                SoftwareLightSpot kLightWorldInModelSpace = (SoftwareLightSpot) m_akLightsWorldInModelSpace[i];

                m_akLightsWorldPosition[i] = new Point3f(kLightWorld.position);
                m_akLightsWorldDirection[i] = new Vector3f(kLightWorld.getDirection());
                m_akLightsWorldPositionInModelSpace[i] = kLightWorldInModelSpace.position;
                m_akLightsWorldDirectionInModelSpace[i] = kLightWorldInModelSpace.getDirection();
            }

            // Point light - copy position
            else if (akLights[i] instanceof SoftwareLightPoint) {
                SoftwareLightPoint kLightWorld = (SoftwareLightPoint) akLights[i];
                SoftwareLightPoint kLightWorldInModelSpace = (SoftwareLightPoint) m_akLightsWorldInModelSpace[i];

                m_akLightsWorldPosition[i] = new Point3f(kLightWorld.position);
                m_akLightsWorldPositionInModelSpace[i] = kLightWorldInModelSpace.position;
            }

            // Directional light - copy direction
            else if (akLights[i] instanceof SoftwareLightDirectional) {
                SoftwareLightDirectional kLightWorld = (SoftwareLightDirectional) akLights[i];
                SoftwareLightDirectional kLightWorldInModelSpace = (SoftwareLightDirectional)
                                                                       m_akLightsWorldInModelSpace[i];

                m_akLightsWorldDirection[i] = new Vector3f(kLightWorld.direction);
                m_akLightsWorldDirectionInModelSpace[i] = kLightWorldInModelSpace.direction;
            }
        }
    }
}
