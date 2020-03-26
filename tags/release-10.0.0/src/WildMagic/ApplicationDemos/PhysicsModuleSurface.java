package WildMagic.ApplicationDemos;

import WildMagic.LibFoundation.Distance.DistanceVector3Segment3;
import WildMagic.LibFoundation.Mathematics.Mathf;
import WildMagic.LibFoundation.Mathematics.Segment3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibPhysics.ParticleSystem.MassSpringArbitrary;
import WildMagic.LibPhysics.ParticleSystem.MassSpringCurve;
import WildMagic.LibPhysics.ParticleSystem.MassSpringSurface;



public class PhysicsModuleSurface extends MassSpringSurface
{
	protected float[] mPhases;
	protected boolean EnableWind = false;
	protected boolean EnableWindChange = false;
	protected Vector3f mGravity, mWind, mDirection;
	protected float mAmplitude, mViscosity;
	
	public PhysicsModuleSurface (int numRows, int numCols, float step, final Vector3f gravity,
			final Vector3f wind, float viscosity, float amplitude)
	{
		super(numRows, numCols, step);
		mGravity = new Vector3f(gravity);
		mWind = new Vector3f(wind);
		mAmplitude = amplitude;
		mViscosity = viscosity;
		

	    mDirection = Vector3f.unitCross( mGravity, mWind);

	    mPhases = new float[mNumParticles];
	    for (int row = 0; row < mNumRows; ++row)
	    {
	        for (int col = 0; col < mNumCols; ++col)
	        {
	            mPhases[GetIndex(row, col)] = (float) (Mathf.UnitRandom()*Math.PI);
	        }
	    }
	}
	

	public void dispose() 
	{
		mPhases = null;
	}

	public Vector3f ExternalAcceleration (int i, float time, final Vector3f[] positions, final Vector3f[] velocities)
	{
		int[] row = new int[1], col = new int[1];
		GetCoordinates(i, row, col);
		Vector3f acceleration = new Vector3f();
		
		if ( row[0] > 0 )
		{
			// Acceleration due to gravity, wind, and viscosity.
			//	    Vector3f acceleration = mGravity + mWind - mViscosity*velocities[i];
			acceleration = Vector3f.add(mGravity, mWind);
			Vector3f temp = Vector3f.scale( - mViscosity, velocities[i] );
			acceleration.add(temp);

			// Add a sinusoidal perturbation.
			float amplitude = (float) (mAmplitude*Math.sin(2.0f*time + mPhases[i]));
			//	    acceleration += amplitude*mDirection;
			acceleration.add( Vector3f.scale(amplitude,mDirection ) );

			if ( row[0] > 1 && row[0] < mNumRows )
			{
				Segment3f seg = new Segment3f( GetPosition( row[0] - 2, col[0] ), GetPosition( row[0], col[0] ) );
				DistanceVector3Segment3 dist = new DistanceVector3Segment3( GetPosition( row[0] - 1, col[0] ), seg );
				if ( Math.abs(dist.Get()) > Mathf.ZERO_TOLERANCE )
				{
					Vector3f dir = Vector3f.sub( dist.GetClosestPoint1(), GetPosition( row[0] - 1, col[0] ) );
					dir.scale( -mViscosity );
					acceleration.add( dir );
				}
			}
		}
		
		if ( col[0] == 0 )
		{
			acceleration.add( Vector3f.UNIT_X );
		}		
		if ( col[0] == mNumColsM1 )
		{
			acceleration.add( Vector3f.UNIT_X_NEG );
		}
		

		if ( row[0] == 0 )
		{
			acceleration.add( Vector3f.UNIT_Z_NEG );
			
			System.err.println( acceleration );
		}		
		if ( row[0] == mNumRowsM1 )
		{
			acceleration.add( Vector3f.UNIT_Z );
		}		
		
	    return acceleration;
	}
}