package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;

public class PointerUpdater<E> {
	
	
		public void Clear(){
			newBase= null;
			oldBase= null;
		    newEnd= null;
		    oldEnd= null;
			preventUpdateFlag=false;
		}
		
		public void Update(Face vp)
		{
			// Ruida, face comparison and addressing ?????????????
			/*
			if(vp>=newBase && vp<newEnd) return;
			assert(vp>=oldBase);
			assert(vp<oldEnd);
			vp=newBase+(vp-oldBase);
		     */
		}
		
		public boolean NeedUpdate() {
			if(oldBase != null && !newBase.equals(oldBase) && !preventUpdateFlag) 
				return true; 
			else return false;
		}

		Face oldBase;
		Face newBase;
		Face newEnd;
		Face oldEnd;
		public boolean preventUpdateFlag; /// when true no update is considered necessary.
	
}