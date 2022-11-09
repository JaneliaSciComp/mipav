package WildMagic.ApplicationDemos;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibPhysics.ParticleSystem.MassSpringArbitrary;



public class PhysicsModule extends MassSpringArbitrary
{

	protected float mViscosity;
	public PhysicsModule (int numParticles, int numSprings, float step, float viscosity)
	{
		super(numParticles, numSprings, step);
		mViscosity = viscosity;
	}
	

	public void dispose() {}

	public Vector3f ExternalAcceleration (int i, float time, final Vector3f[] positions, final Vector3f[] velocities)
	{
		return Vector3f.scale( -mViscosity, velocities[i] );
	}
}