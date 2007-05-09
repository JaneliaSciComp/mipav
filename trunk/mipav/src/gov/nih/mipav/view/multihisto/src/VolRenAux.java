package gov.nih.mipav.view.multihisto.src;

public class VolRenAux {

     private gluvvGlobal gluvv;
     private VectorMath math = new VectorMath();

    public VolRenAux(gluvvGlobal _gluvv) {
      gluvv = _gluvv;
    }
    //====================================================== PostCNorms
//=================================================================
// Compute the post-classification normals
    public void VRA_PostCNorms(byte[] gradout)
    {
            if((gluvv.tf.ptexsz[0] != 256)||(gluvv.tf.ptexsz[1] != 256)||(gluvv.tf.ptexsz[2] != 1))
            {
                    System.err.println("Error: VolRenAux::VRA_PostCNorms()-");
                    System.err.println("   This gradient measure is not implemented yet");
            }

            int sx = gluvv.mv.xiSize;
            int sy = gluvv.mv.yiSize;
            int sz = gluvv.mv.ziSize;

            byte[] data = null;

            if(gluvv.mv.numSubVols != 1)
            {
                    if(gluvv.mv.wholeVol == null){
                            System.err.println("Error: VolRenAux::VRA_PostCNorms()-");
                            System.err.println("  This gradient measure is not implemented for bricked volumes");
                    } else {
                            data = gluvv.mv.wholeVol.currentData;
                    }
            }
            else
            {
                    data = gluvv.mv.volumes[0].currentData;
            }

      byte[] vol = new byte[sx*sy*sz];

      int e = gluvv.mv.nelts;

      int i;


        if ( gluvv.dmode == gluvvDataMode.GDM_V1 ||  //one byte texture
            gluvv.dmode == gluvvDataMode.GDM_VGH_V ) {
          for (i = 0; i < sx * sy * sz; ++i) {
            vol[i] = gluvv.volren.deptex[data[i * e] * 4 + 3];
          }
        } else if ( gluvv.dmode == gluvvDataMode.GDM_V1G || //two byte texture
                    gluvv.dmode == gluvvDataMode.GDM_V2 ||
                    gluvv.dmode == gluvvDataMode.GDM_VGH_VG ) {
        } else if ( gluvv.dmode == gluvvDataMode.GDM_V1GH ||  //3 or 4 byte data == 4 byte texture
                    gluvv.dmode == gluvvDataMode.GDM_V2G ||
                    gluvv.dmode == gluvvDataMode.GDM_V2GH ||
                    gluvv.dmode == gluvvDataMode.GDM_V3 ||
                    gluvv.dmode == gluvvDataMode.GDM_V3G ||
                    gluvv.dmode == gluvvDataMode.GDM_V4 ||
                    gluvv.dmode == gluvvDataMode.GDM_VGH ) {
        }


      float[] gv = new float[sx*sy*sz*3];
      byte[] m = new byte[sx*sy*sz];

      math.AGradArb(gv, sx, sy, sz, 1, 1, data);
      // AGradArb<unsigned char>(gv, sx, sy, sz, 1, 1, data);
      //derivative3D(m, gv, sx, sy, sz, vol);
      //cerr << "Blurring normals" << endl;
      //blurV3D(gv, (float)1.0, (float).3, (float).25, (float).2, sx, sy, sz);
      math.scalebias(gradout, gv, sx, sy, sz);

      gv = null;
      m = null;
      vol = null;



    }




}
