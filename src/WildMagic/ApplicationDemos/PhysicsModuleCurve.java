package WildMagic.ApplicationDemos;

import WildMagic.LibFoundation.Mathematics.Mathf;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibPhysics.ParticleSystem.MassSpringArbitrary;
import WildMagic.LibPhysics.ParticleSystem.MassSpringCurve;



public class PhysicsModuleCurve extends MassSpringCurve
{

	protected boolean EnableWind = false;
	protected boolean EnableWindChange = false;
	protected Vector3f mGravity, mWind;
	protected float mWindChangeAmplitude, mViscosity;
	
	public PhysicsModuleCurve (int numParticles, float step, final Vector3f gravity,
			final Vector3f wind, float windChangeAplitude, float viscosity)
	{
		super(numParticles, step);
		mGravity = new Vector3f(gravity);
		mWind = new Vector3f(wind);
		mWindChangeAmplitude = windChangeAplitude;
		mViscosity = viscosity;
	}
	

	public void dispose() {}

	public Vector3f ExternalAcceleration (int i, float time, final Vector3f[] positions, final Vector3f[] velocities)
	{
	    // Acceleration due to gravity.
	    Vector3f acceleration = new Vector3f(mGravity);

	    // Acceleration due to wind.
	    if (EnableWind)
	    {
	        if (EnableWindChange)
	        {
	            // Generate random direction close to last one.
	            Vector3f U = new Vector3f();
	            Vector3f V = new Vector3f();
	            Vector3f W = new Vector3f(mWind);
	            float length = W.normalize();
	            Vector3f.generateComplementBasis(U, V, W);
	            float uDelta = mWindChangeAmplitude*Mathf.SymmetricRandom();
	            float vDelta = mWindChangeAmplitude*Mathf.SymmetricRandom();
	            U.scale(uDelta);
	            V.scale(vDelta);
	            U.add(V);
	            W.add(U);
	            W.normalize();
	            mWind.copy(W);
	            mWind.scale(length);
	        }
	        acceleration.add(mWind);
	    }

	    // Add in a friction term.  Otherwise the system tends to be "stiff"
	    // (in the numerical stability sense) and leads to oscillatory behavior.
	    acceleration.add( Vector3f.scale( -mViscosity, velocities[i] ) );

	    return acceleration;
	}
}