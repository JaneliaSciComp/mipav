package gov.nih.mipav.view.renderer;

import javax.vecmath.*;

/**
 * The class for spot lights in a general lighting system to calculate
 * diffuse and specular colors at a point in space.
 *
 * See ShearWarpRendering.pdf for a detailed description of the lighting
 * model.
 */

public class SoftwareLightSpot
    extends SoftwareLightPoint {

    /**
     * Copy (deep) constructor.
     * @param kThat Light What to make "this" light like.
     */
    public SoftwareLightSpot (SoftwareLightSpot kThat) {
        super(kThat);
        m_kAxis = new Vector3f(kThat.m_kAxis);
        setAngle(kThat.getAngle());
        setExponent(kThat.getExponent());
    }

    /**
     * Construct a spot light.  The default colors are all black, the
     * intensity is zero, and the light is off.  The attenuation parameters
     * are initialized to 1 (no attenuation).  The class provides accessor
     * methods for attenuation.
     */
    public SoftwareLightSpot() {
        m_kAxis = new Vector3f(0.0f, 0.0f, 0.0f);
        setAngle((float)Math.PI);
        setExponent(0.0f);
    }

    /**
     * Create a deep copy of this Light instance.
     * @return Light Deep copy of this Light instance.
     */
    public SoftwareLight copy() {
        return new SoftwareLightSpot(this);
    }

    /**
     * Compute the contribution of color from this light on the specified
     * material with specified vertex properties.
     * @param kMaterial SoftwareMaterial Default properties for the material
     * including emissive, ambient, diffuse, and specular colors along
     * with the specular shininess which can be used to disable specular
     * highlighting.
     * @param kVertexProperty SoftwareVertexProperty Properties of the
     * vertex which include its position and normal vector along with
     * optional material property overrides for the diffuse and specular
     * colors.
     * @param kEye Point3f The location of the observer in the coordinate
     * system defined for the light.
     * @return Color3f Reference to the computed color contribution by
     * this light with this vertex and its material properties.  The
     * instance returned is owned by this light.
     */
    public final Color3f colorOf(SoftwareMaterial kMaterial,
                           SoftwareVertexProperty kVertexProperty,
                           Point3f kEye) {
       /*
      initView(kEye, kVertexProperty);

      m_kColor.set(0.0f, 0.0f, 0.0f);

      Vector3f R = new Vector3f();
      Vector3f L = new Vector3f();
      Vector3f V = new Vector3f();
      Vector3f N = new Vector3f();
      Vector3f D = new Vector3f();
      float cos_theta, cos_alpha;
      Color3f diffuseColor = new Color3f();
      Color3f specularColor = new Color3f();
      Color3f ambientColor = new Color3f();
      Color3f result = new Color3f();
      Color3f vertexDiffuse;
      float fov_factor;

      V.sub(kVertexProperty.getPosition(), position);
      V.normalize();
      D = getDirection();
      D.negate();

      fov_factor = V.dot(D);

      if ( fov_factor <= 0.0 || fov_factor < m_fCosAngle ) {
        V.negate();
        fov_factor = 0.0f;
      } else {
        fov_factor = ((float)Math.cos((1.0 - fov_factor) * Math.PI / (1.0 - m_fCosAngle))  * 0.5f + 0.5f);
        V.negate();
        // fov_factor = 1.0f;
      }

      N = kVertexProperty.getNormal();
      N.normalize();

      L.sub(position, kVertexProperty.getPosition());
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

        diffuseColor.scale(fov_factor * cos_theta); // 1.0f for diffuse reflection factor
        result.add(diffuseColor);
      }

      if (cos_alpha > 0) {
        specularColor.set(specular);
        specularColor.scale(fov_factor * (float) Math.pow(cos_alpha, kMaterial.shininess)); // 0.6f for specular reflection factor
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
      */

        initView(kEye, kVertexProperty);

        // no ambient light
        m_kColor.set(0.0f, 0.0f, 0.0f);

        // Compute V-P for vertex V and light position P.  The normalize
        // function exists to make the direction unit-length, but we happen
        // to need the actual length for attenuation.  (Java3D's normalize()
        // does not return the length of the input.)
        m_kDirection.sub(kVertexProperty.getPosition(), position);
        float fDistance = normalize(m_kDirection);
        if (m_fExponent > 0.0f)
            computeSpot(m_kDirection);

        // attenuate based on the spot cone
        if (m_fSpot > 0.0f) {
            // attenuate based on the distance from light to vertex
            if (m_bAttenuate)
                computeAttenuation(fDistance);

            float fDdN = addDiffuse(kMaterial, kVertexProperty, m_kDirection);
            if (fDdN < 0.0f && kMaterial.hasSpecular()) {
                addSpecular(kMaterial, kVertexProperty, m_kDirection, fDdN);
            }
        }

        m_kColor.scale(intensity);
        return m_kColor;

    }

    /**
     * Set the light direction
     * @param v   light direction
     */
    public void setDirection(Vector3f v) {
        m_kAxis.set(v);
        normalize(m_kAxis);
    }

    /**
     * Set the light direction.
     * @param fX float X coordinate of the light direction vector.
     * @param fY float Y coordinate of the light direction vector.
     * @param fZ float Z coordinate of the light direction vector.
     */
    public void setDirection(float fX, float fY, float fZ) {
        m_kAxis.set(fX, fY, fZ);
        normalize(m_kAxis);
    }

    /**
     * Get the light direction
     * @return m_kDirection  light direction
     */
    public Vector3f getDirection() {
        return m_kAxis;
    }

    /**
     * Set the spot cone angle.  The angle must be in [0,pi).
     *
     * @param fAngle the spot cone angle
     */
    public void setAngle(float fAngle) {
        m_fAngle = fAngle;
        m_fCosAngle = (float) Math.cos(m_fAngle);
    }

    /**
     * Get the spot cone angle.
     *
     * @return the spot cone angle
     */
    public float getAngle() {
        return m_fAngle;
    }

    /**
     * Set the spot cone exponent.  The exponent must be nonnegative.  If it
     * is zero, the light is completely attenuated.
     *
     * @param fExponent the spot cone exponent
     */
    public void setExponent(float fExponent) {
        m_fExponent = fExponent;
        m_fSpot = 1.0f;
    }

    /**
     * Get the spot cone exponent.
     *
     * @return the spot cone exponent
     */
    public float getExponent() {
        return m_fExponent;
    }

    /**
     * Compute the attenuation due to the spot cone geometry.
     *
     * @param kDirection the unit-length direction from the light's position
     *    to the vertex
     */
    protected void computeSpot(Vector3f kDirection) {
        float fDdA = kDirection.dot(m_kAxis);
        if (fDdA >= m_fCosAngle)
            m_fSpot = (float) Math.pow(fDdA, m_fExponent);
        else
            m_fSpot = 0.0f;
    }

    // the spot cone unit-length axis
    protected final Vector3f m_kAxis;

    // the spot cone angle and exponent
    protected float m_fAngle;
    protected float m_fExponent;

    // cosine of the spot cone angle, to avoid computing cosine repeatedly
    protected float m_fCosAngle;
}
