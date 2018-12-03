package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJComponentGraph;
import gov.nih.mipav.view.ViewJFrameGraph;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;

/**
Copyright (c) 2006-2012 Filip Wasilewski <http://en.ig.ma/>
Copyright (c) 2012-2017 The PyWavelets Developers <https://github.com/PyWavelets/pywt>

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

public  class PyWavelets extends AlgorithmBase {
	// Syntax defaults to the CDF 9/7 wavelets used in JPEG2000
	// Author of original code: Brian Moore
	// brimoor@umich.edu
	
	private final int CDF_type = 1;
	
	private final int spline_type = 2;
	
	private final int waveletMatrix_method = 1;
	
	private final int convolution_method = 2;
	
	/** D1MACH(4). */
    private double epsilon = Double.NaN;
    
    // If you used BWDesignTool to generate a biorthogonal wavelet filter and saved the output variables
    // [h, ht, g, gt, inds_h, inds_ht, inds_g, inds_gt] you may pass them directly into BiorthogonalWavellets.
    private double h[];
    
    private double ht[];
    
    private int inds_h[];
    
    private int inds_ht[];
    
    private double g[];
    
    private double gt[];
    
    private int inds_g[];
    
    private int inds_gt[];
    
    private ModelImage transformImage;
    
    private ModelImage compressedTransformImage;
    
    private ModelImage reconstructedImage;
    
    private int iterations;
    
    // 0 <= compressionFactor <= 1.
    // Alternatively, when compressionFactor == -1, the lowest frequency approximation is retained.
    private double compressionFactor;
    
    // Type can be "spline" or "CDF" and controls the type of biorthogonal wavelet filter used.
    private int type;
    
    // 2*(l+R) + 1 and 2*(lt + C) + 1 are the lengths of the lowpass biorthogonal filters h and ht,
    // where R and C are the number of real and complex zeros of P(t), deg(P) = l + lt - 1,
    // respectively when type == "CDF".
    private int l;
    
    private int lt;
    
    // 2*N+Nt-1 and Nt+1 are the lengths of the lowpass biorthogonal spline filters h and ht,
    // respectively when type == "spline".
    private int N;
    
    private int Nt;
    
    // Plot result or not
    private boolean display = true;
    
    // Forward transform computation method
    // convolution_method or waveletMatrix_method
    private int method = convolution_method;
    
    private double hRealRoots[];
    
    private double hComplexRoots[][];
    
    private double htRealRoots[];
    
    private double htComplexRoots[][];
    
    private double P[];
    
    private double h1[];
    
    private double ht1[];
    
    private String str;
    
    private double HwReal[];
    private double HwImag[];
    private float absHw[];
    
    private double HtwReal[];
    private double HtwImag[];
    private float absHtw[];
    
    private double GwReal[];
    private double GwImag[];
    private float absGw[];
    
    private double GtwReal[];
    private double GtwImag[];
    private float absGtw[];
    
    private class ArrayInfo {
        int shape[];
        int strides[];
        int ndim;
    }

    private enum Coefficient {
        COEF_APPROX (0),
        COEF_DETAIL (1);
    	private final int type;
        Coefficient(int type) {
        	this.type = type;
        }
        private int type() {return type;}
    }

    private enum DiscreteTransformType {
        DWT_TRANSFORM (0),
        SWT_TRANSFORM (1);
    	private final int type;
        DiscreteTransformType (int type) {
        	this.type = type;
        }
        private int type() {return type;}
    }

    /* Signal extension modes */
    private enum MODE {
           MODE_INVALID (-1),
           MODE_ZEROPAD (0),   /* default, signal extended with zeros */
           MODE_SYMMETRIC(1),     /* signal extended symmetrically (mirror)
                                * also known as half-sample symmetric
                                * For extensions greater than signal length,
                                * mirror back and forth:
                                * 2 3 3 2 1 | 1 2 3 | 3 2 1 1 2
                                */
           MODE_CONSTANT_EDGE(2), /* signal extended with the border value */
           MODE_SMOOTH(3),        /* linear extrapolation (first derivative) */
           MODE_PERIODIC(4),      /* signal is treated as being periodic */
           MODE_PERIODIZATION(5), /* signal is treated as being periodic, minimal output length */
           MODE_REFLECT(6),       /* signal extended symmetrically (reflect)
                                * also known as whole-sample symmetric
                                * For extensions greater than signal length,
                                * reflect back and forth without repeating edge values:
                                * 1 2 3 2 | 1 2 3 | 2 1 2 3
                                */
           MODE_ANTISYMMETRIC(7),  /* antisymmetric version of "MODE_SYMMETRIC"
                                 * also known as half-sample antisymmetric
                                 * 2 3 -3 -2 -1 | 1 2 3 | -3 -2 -1 1 2
                                 */
           MODE_ANTIREFLECT(8),    /* antisymmetric version of "MODE_REFLECT"
                                 * also known as whole-sample antisymmetric
                                 * 0 -1 -2 -1 0 | 1 2 3 | 4 5 6 5 4
                                 */
           MODE_MAX(9);
           
           private final int type;
           MODE(int type) {
           	this.type = type;
           }
           private int type() {return type;}
           
    }
    
    /* Wavelet symmetry properties */
    private enum SYMMETRY{
        UNKNOWN (-1),
        ASYMMETRIC (0),
        NEAR_SYMMETRIC (1),
        SYMMETRIC (2),
        ANTI_SYMMETRIC (3);
        
        private final int type;
        SYMMETRY(int type) {
        	this.type = type;
        }
        private int type() {return type;}
    }
    
    /* Wavelet name */
    private enum WAVELET_NAME {
        HAAR,
        RBIO,
        DB,
        SYM,
        COIF,
        BIOR,
        DMEY,
        GAUS,
        MEXH,
        MORL,
        CGAU,
        SHAN,
        FBSP,
        CMOR;
    }
    
    private int is_discrete_wavelet(WAVELET_NAME name)
    {
        switch(name){
            case HAAR:
                return 1;
            case RBIO:
                return 1;
            case DB:
                return 1;
            case SYM:
                return 1;
            case COIF:
                return 1;
            case BIOR:
                return 1;
            case DMEY:
                return 1;
            case GAUS:
                return 0;
            case MEXH:
                return 0;
            case MORL:
                return 0;
            case CGAU:
                return 0;
            case SHAN:
                return 0;
            case FBSP:
                return 0;
            case CMOR:
                return 0;
            default:
                return -1;
        }

    }
    
    /* Wavelet structure holding pointers to filter arrays and property attributes */
    private class BaseWavelet {
        /* Wavelet properties */

        int support_width;

        SYMMETRY symmetry;

        int orthogonal = 1;
        int biorthogonal = 1;
        int compact_support = 1;

        int _builtin;
        String family_name;
        String short_name;
    }
    
    private class DiscreteWavelet {
        BaseWavelet base;
        double dec_hi_double[];  /* highpass decomposition */
        double dec_lo_double[];  /* lowpass decomposition */
        double rec_hi_double[];  /* highpass reconstruction */
        double rec_lo_double[];  /* lowpass reconstruction */
        float dec_hi_float[];
        float dec_lo_float[];
        float rec_hi_float[];
        float rec_lo_float[];
        int dec_len;   /* length of decomposition filter */
        int rec_len;   /* length of reconstruction filter */

        int vanishing_moments_psi;
        int vanishing_moments_phi;
    } 
    
    private class ContinuousWavelet {

        BaseWavelet base;
        float lower_bound;
        float upper_bound;
        /* Parameters for shan, fbsp, cmor*/
        int complex_cwt;
        float center_frequency;
        float bandwidth_frequency;
        int fbsp_order;
    }
    
    private void SWAP_FLOAT_ARRAY(float x[], float y[]) {
    	float tmp[];
    	tmp = x;
    	x = y;
    	y = tmp;
    }
    
    private void SWAP_DOUBLE_ARRAY(double x[], double y[]) {
    	double tmp[];
    	tmp = x;
    	x = y;
    	y = tmp;
    }
    
    static final float[] db1_float = new float[]{
    		   7.071067811865475244008443621048490392848359376884740365883398e-01f,
    		 7.071067811865475244008443621048490392848359376884740365883398e-01f
    };
    
    static final double[] db1_double = new double[]{
 		   7.071067811865475244008443621048490392848359376884740365883398e-01,
 		 7.071067811865475244008443621048490392848359376884740365883398e-01
    };
    
    static final float[] db2_float = new float[]{
    		 4.829629131445341433748715998644486838169524195042022752011715e-01f,
    		 8.365163037378079055752937809168732034593703883484392934953414e-01f,
    		 2.241438680420133810259727622404003554678835181842717613871683e-01f,
    		-1.294095225512603811744494188120241641745344506599652569070016e-01f
    };
    
    static final double[] db2_double = new double[]{
   		 4.829629131445341433748715998644486838169524195042022752011715e-01,
   		 8.365163037378079055752937809168732034593703883484392934953414e-01,
   		 2.241438680420133810259727622404003554678835181842717613871683e-01,
   		-1.294095225512603811744494188120241641745344506599652569070016e-01
    };
    
    static final float db3_float[] = new float[] {
    		 3.326705529500826159985115891390056300129233992450683597084705e-01f,
    		 8.068915093110925764944936040887134905192973949948236181650920e-01f,
    		 4.598775021184915700951519421476167208081101774314923066433867e-01f,
    		-1.350110200102545886963899066993744805622198452237811919756862e-01f,
    		-8.544127388202666169281916918177331153619763898808662976351748e-02f,
    		 3.522629188570953660274066471551002932775838791743161039893406e-02f
    };
   
    static final double db3_double[] = new double[] {
  		 3.326705529500826159985115891390056300129233992450683597084705e-01,
  		 8.068915093110925764944936040887134905192973949948236181650920e-01,
  		 4.598775021184915700951519421476167208081101774314923066433867e-01,
  		-1.350110200102545886963899066993744805622198452237811919756862e-01,
  		-8.544127388202666169281916918177331153619763898808662976351748e-02,
  		 3.522629188570953660274066471551002932775838791743161039893406e-02
    };
    
    static final float db4_float[] = new float[] {
    		 2.303778133088965008632911830440708500016152482483092977910968e-01f,
    		 7.148465705529156470899219552739926037076084010993081758450110e-01f,
    		 6.308807679298589078817163383006152202032229226771951174057473e-01f,
    		-2.798376941685985421141374718007538541198732022449175284003358e-02f,
    		-1.870348117190930840795706727890814195845441743745800912057770e-01f,
    		 3.084138183556076362721936253495905017031482172003403341821219e-02f,
    		 3.288301166688519973540751354924438866454194113754971259727278e-02f,
    		-1.059740178506903210488320852402722918109996490637641983484974e-02f
    };
    
    static final double db4_double[] = new double[] {
   		 2.303778133088965008632911830440708500016152482483092977910968e-01,
   		 7.148465705529156470899219552739926037076084010993081758450110e-01,
   		 6.308807679298589078817163383006152202032229226771951174057473e-01,
   		-2.798376941685985421141374718007538541198732022449175284003358e-02,
   		-1.870348117190930840795706727890814195845441743745800912057770e-01,
   		 3.084138183556076362721936253495905017031482172003403341821219e-02,
   		 3.288301166688519973540751354924438866454194113754971259727278e-02,
   		-1.059740178506903210488320852402722918109996490637641983484974e-02
    };
    
    static final float db5_float[] = new float[] {
		 1.601023979741929144807237480204207336505441246250578327725699e-01f,
		 6.038292697971896705401193065250621075074221631016986987969283e-01f,
		 7.243085284377729277280712441022186407687562182320073725767335e-01f,
		 1.384281459013207315053971463390246973141057911739561022694652e-01f,
		-2.422948870663820318625713794746163619914908080626185983913726e-01f,
		-3.224486958463837464847975506213492831356498416379847225434268e-02f,
		 7.757149384004571352313048938860181980623099452012527983210146e-02f,
		-6.241490212798274274190519112920192970763557165687607323417435e-03f,
		-1.258075199908199946850973993177579294920459162609785020169232e-02f,
		 3.335725285473771277998183415817355747636524742305315099706428e-03f
    };
    
    static final double db5_double[] = new double[] {
 		 1.601023979741929144807237480204207336505441246250578327725699e-01,
 		 6.038292697971896705401193065250621075074221631016986987969283e-01,
 		 7.243085284377729277280712441022186407687562182320073725767335e-01,
 		 1.384281459013207315053971463390246973141057911739561022694652e-01,
 		-2.422948870663820318625713794746163619914908080626185983913726e-01,
 		-3.224486958463837464847975506213492831356498416379847225434268e-02,
 		 7.757149384004571352313048938860181980623099452012527983210146e-02,
 		-6.241490212798274274190519112920192970763557165687607323417435e-03,
 		-1.258075199908199946850973993177579294920459162609785020169232e-02,
 		 3.335725285473771277998183415817355747636524742305315099706428e-03
    };
    
    static final float db6_float[] = new float[] {
		 1.115407433501094636213239172409234390425395919844216759082360e-01f,
		 4.946238903984530856772041768778555886377863828962743623531834e-01f,
		 7.511339080210953506789344984397316855802547833382612009730420e-01f,
		 3.152503517091976290859896548109263966495199235172945244404163e-01f,
		-2.262646939654398200763145006609034656705401539728969940143487e-01f,
		-1.297668675672619355622896058765854608452337492235814701599310e-01f,
		 9.750160558732304910234355253812534233983074749525514279893193e-02f,
		 2.752286553030572862554083950419321365738758783043454321494202e-02f,
		-3.158203931748602956507908069984866905747953237314842337511464e-02f,
		 5.538422011614961392519183980465012206110262773864964295476524e-04f,
		 4.777257510945510639635975246820707050230501216581434297593254e-03f,
		-1.077301085308479564852621609587200035235233609334419689818580e-03f
    };
    
    static final double db6_double[] = new double[] {
   		 1.115407433501094636213239172409234390425395919844216759082360e-01,
   		 4.946238903984530856772041768778555886377863828962743623531834e-01,
   		 7.511339080210953506789344984397316855802547833382612009730420e-01,
   		 3.152503517091976290859896548109263966495199235172945244404163e-01,
   		-2.262646939654398200763145006609034656705401539728969940143487e-01,
   		-1.297668675672619355622896058765854608452337492235814701599310e-01,
   		 9.750160558732304910234355253812534233983074749525514279893193e-02,
   		 2.752286553030572862554083950419321365738758783043454321494202e-02,
   		-3.158203931748602956507908069984866905747953237314842337511464e-02,
   		 5.538422011614961392519183980465012206110262773864964295476524e-04,
   		 4.777257510945510639635975246820707050230501216581434297593254e-03,
   		-1.077301085308479564852621609587200035235233609334419689818580e-03
    };
    
    static final float db7_float[] = new float[] {
		 7.785205408500917901996352195789374837918305292795568438702937e-02f,
		 3.965393194819173065390003909368428563587151149333287401110499e-01f,
		 7.291320908462351199169430703392820517179660611901363782697715e-01f,
		 4.697822874051931224715911609744517386817913056787359532392529e-01f,
		-1.439060039285649754050683622130460017952735705499084834401753e-01f,
		-2.240361849938749826381404202332509644757830896773246552665095e-01f,
		 7.130921926683026475087657050112904822711327451412314659575113e-02f,
		 8.061260915108307191292248035938190585823820965629489058139218e-02f,
		-3.802993693501441357959206160185803585446196938467869898283122e-02f,
		-1.657454163066688065410767489170265479204504394820713705239272e-02f,
		 1.255099855609984061298988603418777957289474046048710038411818e-02f,
		 4.295779729213665211321291228197322228235350396942409742946366e-04f,
		-1.801640704047490915268262912739550962585651469641090625323864e-03f,
		 3.537137999745202484462958363064254310959060059520040012524275e-04f
    };
    
    static final double db7_double[] = new double[] {
		 7.785205408500917901996352195789374837918305292795568438702937e-02,
		 3.965393194819173065390003909368428563587151149333287401110499e-01,
		 7.291320908462351199169430703392820517179660611901363782697715e-01,
		 4.697822874051931224715911609744517386817913056787359532392529e-01,
		-1.439060039285649754050683622130460017952735705499084834401753e-01,
		-2.240361849938749826381404202332509644757830896773246552665095e-01,
		 7.130921926683026475087657050112904822711327451412314659575113e-02,
		 8.061260915108307191292248035938190585823820965629489058139218e-02,
		-3.802993693501441357959206160185803585446196938467869898283122e-02,
		-1.657454163066688065410767489170265479204504394820713705239272e-02,
		 1.255099855609984061298988603418777957289474046048710038411818e-02,
		 4.295779729213665211321291228197322228235350396942409742946366e-04,
		-1.801640704047490915268262912739550962585651469641090625323864e-03,
		 3.537137999745202484462958363064254310959060059520040012524275e-04
   };
    
   static final float db8_float[] = new float[] {
		 5.441584224310400995500940520299935503599554294733050397729280e-02f,
		 3.128715909142999706591623755057177219497319740370229185698712e-01f,
		 6.756307362972898068078007670471831499869115906336364227766759e-01f,
		 5.853546836542067127712655200450981944303266678053369055707175e-01f,
		-1.582910525634930566738054787646630415774471154502826559735335e-02f,
		-2.840155429615469265162031323741647324684350124871451793599204e-01f,
		 4.724845739132827703605900098258949861948011288770074644084096e-04f,
		 1.287474266204784588570292875097083843022601575556488795577000e-01f,
		-1.736930100180754616961614886809598311413086529488394316977315e-02f,
		-4.408825393079475150676372323896350189751839190110996472750391e-02f,
		 1.398102791739828164872293057263345144239559532934347169146368e-02f,
		 8.746094047405776716382743246475640180402147081140676742686747e-03f,
		-4.870352993451574310422181557109824016634978512157003764736208e-03f,
		-3.917403733769470462980803573237762675229350073890493724492694e-04f,
		 6.754494064505693663695475738792991218489630013558432103617077e-04f,
		-1.174767841247695337306282316988909444086693950311503927620013e-04f
    };
   
   static final double db8_double[] = new double[] {
		 5.441584224310400995500940520299935503599554294733050397729280e-02,
		 3.128715909142999706591623755057177219497319740370229185698712e-01,
		 6.756307362972898068078007670471831499869115906336364227766759e-01,
		 5.853546836542067127712655200450981944303266678053369055707175e-01,
		-1.582910525634930566738054787646630415774471154502826559735335e-02,
		-2.840155429615469265162031323741647324684350124871451793599204e-01,
		 4.724845739132827703605900098258949861948011288770074644084096e-04,
		 1.287474266204784588570292875097083843022601575556488795577000e-01,
		-1.736930100180754616961614886809598311413086529488394316977315e-02,
		-4.408825393079475150676372323896350189751839190110996472750391e-02,
		 1.398102791739828164872293057263345144239559532934347169146368e-02,
		 8.746094047405776716382743246475640180402147081140676742686747e-03,
		-4.870352993451574310422181557109824016634978512157003764736208e-03,
		-3.917403733769470462980803573237762675229350073890493724492694e-04,
		 6.754494064505693663695475738792991218489630013558432103617077e-04,
		-1.174767841247695337306282316988909444086693950311503927620013e-04
	};
    
    /*DiscreteWavelet discrete_wavelet(WAVELET_NAME name, int order)
    {
    	int tmpInt;
    	float tmpFloat;
    	double tmpDouble;
        DiscreteWavelet w;
        // Haar wavelet
        if(name == HAAR){

            // the same as db1
            w = discrete_wavelet(WAVELET_NAME.DB, 1);
            w.base.family_name = "Haar";
            w.base.short_name = "haar";
            return w;

        // Reverse biorthogonal wavelets family
        } else if (name == RBIO) {
            // rbio is like bior, only with switched filters 
            w = discrete_wavelet(WAVELET_NAME.BIOR, order);
            if (w == null) return null;

            SWAP(size_t, w->dec_len, w->rec_len);
            tmpInt = w.dec_len;
            w.dec_len = w.rec_len;
            w.rec_len = tmpInt;
            SWAP_FLOAT_ARRAY(w.rec_lo_float, w.dec_lo_float);
            SWAP_FLOAT_ARRAY(w.rec_hi_float, w.dec_hi_float);
            SWAP_DOUBLE_ARRAY(w.rec_lo_double, w.dec_lo_double);
            SWAP_DOUBLE_ARRRAY(w.rec_hi_double, w.dec_hi_double);

            {
                int i, j;
                for(i = 0, j = w.rec_len - 1; i < j; i++, j--){
                    tmpFloat = w.rec_lo_float[i];
                    w.rec_lo_float[i] = w.rec_lo_float[j];
                    w.rec_lo_float[j] = tmpFloat;
                    tmpFloat = w.rec_hi_float[i];
                    w.rec_hi_float[i] = w.rec_hi_float[j];
                    w.rec_hi_float[j] = tmpFloat;
                    tmpFloat = w.dec_lo_float[i];
                    w.dec_lo_float[i] = w.dec_lo_float[j];
                    w.dec_lo_float[j] = tmpFloat;
                    tmpFloat = w.dec_hi_float[i];
                    w.dec_hi_float[i] = w.dec_hi_float[j];
                    w.dec_hi_float[j] = tmpFloat;

                    tmpDouble = w.rec_lo_double[i];
                    w.rec_lo_double[i] = w.rec_lo_double[j];
                    w.rec_lo_double[j] = tmpDouble;
                    tmpDouble = w.rec_hi_double[i];
                    w.rec_hi_double[i] = w.rec_hi_double[j];
                    w.rec_hi_double[j] = tmpDouble;
                    tmpDouble = w.dec_lo_double[i];
                    w.dec_lo_double[i] = w.dec_lo_double[j];
                    w.dec_lo_double[j] = tmpDouble;
                    tmpDouble = w.dec_hi_double[i];
                    w.dec_hi_double[i] = w.dec_hi_double[j];
                    w.dec_hi_double[j] = tmpDouble;

                }
            }

            w.base.family_name = "Reverse biorthogonal";
            w.base.short_name = "rbio";

            return w;
        }

        switch(name){
            // Daubechies wavelets family
            case DB: {
                int coeffs_idx = order - 1;
                if (coeffs_idx >= NELEMS(db_float) ||
                    coeffs_idx >= NELEMS(db_double))
                    return NULL;
                w = blank_discrete_wavelet(2 * order);
                if(w == NULL) return NULL;

                w->vanishing_moments_psi = order;
                w->vanishing_moments_phi = 0;
                w->base.support_width = 2*order - 1;
                w->base.orthogonal = 1;
                w->base.biorthogonal = 1;
                w->base.symmetry = ASYMMETRIC;
                w->base.compact_support = 1;
                w->base.family_name = "Daubechies";
                w->base.short_name = "db";
                {
                    size_t i;
                    for(i = 0; i < w->rec_len; ++i){
                        w->rec_lo_float[i] = db_float[coeffs_idx][i];
                        w->dec_lo_float[i] = db_float[coeffs_idx][w->dec_len-1-i];
                        w->rec_hi_float[i] = ((i % 2) ? -1 : 1)
                          * db_float[coeffs_idx][w->dec_len-1-i];
                        w->dec_hi_float[i] = (((w->dec_len-1-i) % 2) ? -1 : 1)
                          * db_float[coeffs_idx][i];
                    }
                }

                {
                    size_t i;
                    for(i = 0; i < w->rec_len; ++i){
                        w->rec_lo_double[i] = db_double[coeffs_idx][i];
                        w->dec_lo_double[i] = db_double[coeffs_idx][w->dec_len-1-i];
                        w->rec_hi_double[i] = ((i % 2) ? -1 : 1)
                          * db_double[coeffs_idx][w->dec_len-1-i];
                        w->dec_hi_double[i] = (((w->dec_len-1-i) % 2) ? -1 : 1)
                          * db_double[coeffs_idx][i];
                    }
                }

                break;
            }

            // Symlets wavelets family
            case SYM: {
                size_t coeffs_idx = order - 2;
                if (coeffs_idx >= NELEMS(sym_float) ||
                    coeffs_idx >= NELEMS(sym_double))
                    return NULL;

                w = blank_discrete_wavelet(2 * order);
                if(w == NULL) return NULL;

                w->vanishing_moments_psi = order;
                w->vanishing_moments_phi = 0;
                w->base.support_width = 2*order - 1;
                w->base.orthogonal = 1;
                w->base.biorthogonal = 1;
                w->base.symmetry = NEAR_SYMMETRIC;
                w->base.compact_support = 1;
                w->base.family_name = "Symlets";
                w->base.short_name = "sym";
                {
                    size_t i;
                    for(i = 0; i < w->rec_len; ++i){
                        w->rec_lo_float[i] = sym_float[coeffs_idx][i];
                        w->dec_lo_float[i] = sym_float[coeffs_idx][w->dec_len-1-i];
                        w->rec_hi_float[i] = ((i % 2) ? -1 : 1)
                          * sym_float[coeffs_idx][w->dec_len-1-i];
                        w->dec_hi_float[i] = (((w->dec_len-1-i) % 2) ? -1 : 1)
                          * sym_float[coeffs_idx][i];
                    }
                }

                {
                    size_t i;
                    for(i = 0; i < w->rec_len; ++i){
                        w->rec_lo_double[i] = sym_double[coeffs_idx][i];
                        w->dec_lo_double[i] = sym_double[coeffs_idx][w->dec_len-1-i];
                        w->rec_hi_double[i] = ((i % 2) ? -1 : 1)
                          * sym_double[coeffs_idx][w->dec_len-1-i];
                        w->dec_hi_double[i] = (((w->dec_len-1-i) % 2) ? -1 : 1)
                          * sym_double[coeffs_idx][i];
                    }
                }
                break;
            }

            // Coiflets wavelets family
            case COIF: {
                size_t coeffs_idx = order - 1;
                if (coeffs_idx >= NELEMS(coif_float) ||
                    coeffs_idx >= NELEMS(coif_double))
                    return NULL;
                w = blank_discrete_wavelet(6 * order);
                if(w == NULL) return NULL;

                w->vanishing_moments_psi = 2*order;
                w->vanishing_moments_phi = 2*order -1;
                w->base.support_width = 6*order - 1;
                w->base.orthogonal = 1;
                w->base.biorthogonal = 1;
                w->base.symmetry = NEAR_SYMMETRIC;
                w->base.compact_support = 1;
                w->base.family_name = "Coiflets";
                w->base.short_name = "coif";
                {
                    size_t i;
                    for(i = 0; i < w->rec_len; ++i){
                        w->rec_lo_float[i] = coif_float[coeffs_idx][i] * sqrt2_float;
                        w->dec_lo_float[i] = coif_float[coeffs_idx][w->dec_len-1-i]
                          * sqrt2_float;
                        w->rec_hi_float[i] = ((i % 2) ? -1 : 1)
                          * coif_float[coeffs_idx][w->dec_len-1-i] * sqrt2_float;
                        w->dec_hi_float[i] = (((w->dec_len-1-i) % 2) ? -1 : 1)
                          * coif_float[coeffs_idx][i] * sqrt2_float;
                    }
                }

                {
                    size_t i;
                    for(i = 0; i < w->rec_len; ++i){
                        w->rec_lo_double[i] = coif_double[coeffs_idx][i] * sqrt2_double;
                        w->dec_lo_double[i] = coif_double[coeffs_idx][w->dec_len-1-i]
                          * sqrt2_double;
                        w->rec_hi_double[i] = ((i % 2) ? -1 : 1)
                          * coif_double[coeffs_idx][w->dec_len-1-i] * sqrt2_double;
                        w->dec_hi_double[i] = (((w->dec_len-1-i) % 2) ? -1 : 1)
                          * coif_double[coeffs_idx][i] * sqrt2_double;
                    }
                }
                break;
            }

            // Biorthogonal wavelets family
            case BIOR: {
                unsigned int N = order / 10, M = order % 10;
                size_t M_idx;
                size_t M_max;
                switch (N) {
                case 1:
                    if (M % 2 != 1 || M > 5) return NULL;
                    M_idx = M / 2;
                    M_max = 5;
                    break;
                case 2:
                    if (M % 2 != 0 || M < 2 || M > 8) return NULL;
                    M_idx = M / 2 - 1;
                    M_max = 8;
                    break;
                case 3:
                    if (M % 2 != 1) return NULL;
                    M_idx = M / 2;
                    M_max = 9;
                    break;
                case 4:
                case 5:
                    if (M != N) return NULL;
                    M_idx = 0;
                    M_max = M;
                    break;
                case 6:
                    if (M != 8) return NULL;
                    M_idx = 0;
                    M_max = 8;
                    break;
                default:
                    return NULL;
                }

                w = blank_discrete_wavelet((N == 1) ? 2 * M : 2 * M + 2);
                if(w == NULL) return NULL;

                w->vanishing_moments_psi = order/10;
                w->vanishing_moments_phi = order % 10;
                w->base.support_width = -1;
                w->base.orthogonal = 0;
                w->base.biorthogonal = 1;
                w->base.symmetry = SYMMETRIC;
                w->base.compact_support = 1;
                w->base.family_name = "Biorthogonal";
                w->base.short_name = "bior";
                {
                    size_t n = M_max - M;
                    size_t i;
                    for(i = 0; i < w->rec_len; ++i){
                        w->rec_lo_float[i] = bior_float[N - 1][0][i+n];
                        w->dec_lo_float[i] = bior_float[N - 1][M_idx+1][w->dec_len-1-i];
                        w->rec_hi_float[i] = ((i % 2) ? -1 : 1)
                          * bior_float[N - 1][M_idx+1][w->dec_len-1-i];
                        w->dec_hi_float[i] = (((w->dec_len-1-i) % 2) ? -1 : 1)
                          * bior_float[N - 1][0][i+n];
                    }
                }

                {
                    size_t n = M_max - M;
                    size_t i;
                    for(i = 0; i < w->rec_len; ++i){
                        w->rec_lo_double[i] = bior_double[N - 1][0][i+n];
                        w->dec_lo_double[i] = bior_double[N - 1][M_idx+1][w->dec_len-1-i];
                        w->rec_hi_double[i] = ((i % 2) ? -1 : 1)
                          * bior_double[N - 1][M_idx+1][w->dec_len-1-i];
                        w->dec_hi_double[i] = (((w->dec_len-1-i) % 2) ? -1 : 1)
                          * bior_double[N - 1][0][i+n];
                    }
                }

                break;
            }

            // Discrete FIR filter approximation of Meyer wavelet
            case DMEY:
                w = blank_discrete_wavelet(62);
                if(w == NULL) return NULL;

                w->vanishing_moments_psi = -1;
                w->vanishing_moments_phi = -1;
                w->base.support_width = -1;
                w->base.orthogonal = 1;
                w->base.biorthogonal = 1;
                w->base.symmetry = SYMMETRIC;
                w->base.compact_support = 1;
                w->base.family_name = "Discrete Meyer (FIR Approximation)";
                w->base.short_name = "dmey";
                {
                    size_t i;
                    for(i = 0; i < w->rec_len; ++i){
                        w->rec_lo_float[i] = dmey_float[i];
                        w->dec_lo_float[i] = dmey_float[w->dec_len-1-i];
                        w->rec_hi_float[i] = ((i % 2) ? -1 : 1)
                          * dmey_float[w->dec_len-1-i];
                        w->dec_hi_float[i] = (((w->dec_len-1-i) % 2) ? -1 : 1)
                          * dmey_float[i];
                    }
                }

                {
                    size_t i;
                    for(i = 0; i < w->rec_len; ++i){
                        w->rec_lo_double[i] = dmey_double[i];
                        w->dec_lo_double[i] = dmey_double[w->dec_len-1-i];
                        w->rec_hi_double[i] = ((i % 2) ? -1 : 1)
                          * dmey_double[w->dec_len-1-i];
                        w->dec_hi_double[i] = (((w->dec_len-1-i) % 2) ? -1 : 1)
                          * dmey_double[i];
                    }
                }
                break;
            default:
                return NULL;
        }
        return w;
    }*/

    
    public PyWavelets(ModelImage transformImage, ModelImage compressedTransformImage, 
    		ModelImage reconstructedImage, ModelImage srcImg, int iterations, double compressionFactor,
    		boolean display, int method,
    		int type, int len, int lent) {
    	super(null, srcImg);
    	this.transformImage = transformImage;
    	this.compressedTransformImage = compressedTransformImage;
    	this.reconstructedImage = reconstructedImage;
    	this.iterations = iterations;
    	this.compressionFactor = compressionFactor;
    	this.display = display;
    	this.method = method;
    	this.type = type;
    	if (type == CDF_type) {
    		l = len;
    		lt = lent;
    	}
    	else if (type == spline_type) {
    		N = len;
    		Nt = lent;
    	}
    }
    
    public void runAlgorithm() {
    	int xDim;
    	int yDim;
    	int zDim;
    	int bound;
    	int maxIters;
    	int factor2inXDim;
    	int factor2inYDim;
    	int xTest;
    	int yTest;
    	int z;
    	int ii;
    	double WMt[][];
    	double WN[][];
    	int length;
    	double buffer[];
    	double IM[][][];
    	double IMt[][][];
    	int x;
    	int y;
    	int yLim;
    	int xLim;
    	int i;
    	double prod[][];
    	double IMtInput[][];
    	double output[][];
    	double IMtc[][][];
    	double IMhat[][][];
    	double coeffs[];
    	double thresh;
    	double WM[][];
    	double WNt[][];
    	double diff;
    	double MSE;
    	
        if (type == spline_type) {
        	//SplineWavelets();
        }
        else if (type == CDF_type) {
        	//CDFWavelets();
        }
        else {
        	MipavUtil.displayError("Incorrect type = " + type);
        	setCompleted(false);
        	return;
        }
        
        // Make sure transforms will be well-defined.
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        buffer = new double[length];
        zDim = 1;
        if (srcImage.getNDims() > 2) {
        	zDim = srcImage.getExtents()[2];
        }
        IM = new double[yDim][xDim][zDim];
        IMt = new double[yDim][xDim][zDim];
        IMtc = new double[yDim][xDim][zDim];
        IMhat = new double[yDim][xDim][zDim];
        
        // Image must have even dimensions
        if (((xDim % 2) == 1) || ((yDim % 2) == 1)) {
        	MipavUtil.displayError("Image must have even dimensions");
        	setCompleted(false);
        	return;
        }
        
        // Determine the maximum number of iterations possible
        bound = Math.max(Math.max(h.length,ht.length),Math.max(g.length,gt.length));
        maxIters = 1;
        while (Math.min(xDim,yDim)/Math.pow(2.0, maxIters) >= bound) {
        	maxIters = maxIters + 1;
        }
        factor2inXDim = 0;
        xTest = xDim;
        while ((xTest % 2) == 0) {
        	factor2inXDim++;
        	xTest = xTest/2;
        }
        factor2inYDim = 0;
        yTest = yDim;
        while ((yTest % 2) == 0) {
        	factor2inYDim++;
        	yTest = yTest/2;
        }
        maxIters = Math.min(maxIters, Math.min(factor2inXDim, factor2inYDim));
        
        // Must have iterations <= maxIters
        if (iterations > maxIters) {
        	MipavUtil.displayError("For that image and those wavelets, you must have iterations <= " + maxIters);
        	setCompleted(false);
        	return;
        }
        
        // Compute iterated wavelet transform
        for (z = 0; z < zDim; z++) {
        	try {
                srcImage.exportData(z * length, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                errorCleanUp("Biorthogonal wavelets: Image(s) locked", true);

                return;
            }
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			IM[y][x][z] = buffer[x + y * xDim];
        			IMt[y][x][z] = IM[y][x][z];
        		}
        	}
            for (ii = 1; ii <= iterations; ii++) {
            	xLim = (int)Math.round(xDim/Math.pow(2.0, (ii-1)));
            	yLim = (int)Math.round(yDim/Math.pow(2.0, (ii-1)));
                
            } // for (ii = 1; ii <= iterations; ii++)
            for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			buffer[x + y * xDim] = IMt[y][x][z];
        			IMtc[y][x][z] = IMt[y][x][z];
         		}
        	}
            try {
                transformImage.importData(z*length, buffer, false);
            } catch (IOException error) {
                buffer = null;
                errorCleanUp("Biorthogonal wavelets: Image(s) locked", true);

                return;
            }
        } // for (z = 0; z < zDim; z++)
        transformImage.calcMinMax();
        
        // Compress image by throwing away compressionFactor % of the wavelet coefficients;
        if (compressionFactor > 0) {
            coeffs = new double[length * zDim];
            for (z = 0; z < zDim; z++) {
            	for (y = 0; y < yDim; y++) {
            		for (x = 0; x < xDim; x++) {
            		    coeffs[length*z + y*xDim + x] = Math.abs(IMt[y][x][z]);
            		}
            	}
            }
            Arrays.sort(coeffs);
            thresh = coeffs[(int)Math.ceil(compressionFactor*length*zDim)-1];
            for (z = 0; z < zDim; z++) {
            	for (i = 0; i < length; i++) {
     	    		buffer[i] = 0;
     	    	}
                for (y = 0; y < yDim; y++) {
            	    for (x = 0; x < xDim; x++) {
            			if (Math.abs(IMt[y][x][z]) <= thresh) {
            				IMtc[y][x][z] = 0;
            			}
            			buffer[x + y * xDim] = IMtc[y][x][z];
            		}
                 }
                 try {
                    compressedTransformImage.importData(z*length, buffer, false);
                 } catch (IOException error) {
                    buffer = null;
                    errorCleanUp("Biorthogonal wavelets: Image(s) locked", true);

                    return;
                 }
            } // for (z = 0; z < zDim; z++)
        } // if (compressionFactor > 0)
        else if (compressionFactor == 0) {
        	for (z = 0; z < zDim; z++) {
            	for (i = 0; i < length; i++) {
     	    		buffer[i] = 0;
     	    	}
                for (y = 0; y < yDim; y++) {
            	    for (x = 0; x < xDim; x++) {
            			buffer[x + y * xDim] = IMtc[y][x][z];
            		}
                 }
                 try {
                    compressedTransformImage.importData(z*length, buffer, false);
                 } catch (IOException error) {
                    buffer = null;
                    errorCleanUp("Biorthogonal wavelets: Image(s) locked", true);

                    return;
                 }
            } // for (z = 0; z < zDim; z++)
        }
        else if (compressionFactor == -1) {
       	    IMtc = new double[yDim][xDim][zDim];
       	    xLim = (int)Math.round(xDim/Math.pow(2.0, iterations));
     	    yLim = (int)Math.round(yDim/Math.pow(2.0, iterations));
     	    for (z = 0; z < zDim; z++) {
     	    	for (i = 0; i < length; i++) {
     	    		buffer[i] = 0;
     	    	}
     	    	for (y = 0; y < yLim; y++) {
     	    		for (x = 0; x < xLim; x++) {
     	    			IMtc[y][x][z] = IMt[y][x][z];
     	    			buffer[x + y * xDim] = IMt[y][x][z];
     	    		}
     	    	}
     	    	try {
                    compressedTransformImage.importData(z*length, buffer, false);
                 } catch (IOException error) {
                    buffer = null;
                    errorCleanUp("Biorthogonal wavelets: Image(s) locked", true);

                    return;
                 }
     	    }
        } // else if (compressionFactor == -1)
        compressedTransformImage.calcMinMax();
        
        // Iteratively reconstruct the image
        for (z = 0; z < zDim; z++) {
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        		    IMhat[y][x][z] = IMtc[y][x][z];	
        		}
        	}
        }
        
        for (z = 0; z < zDim; z++) {
        	for (i = 0; i < length; i++) {
        		buffer[i] = 0;
        	}
        	
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			buffer[x + y * xDim] = IMhat[y][x][z];
         		}
        	}
            try {
                reconstructedImage.importData(z*length, buffer, false);
            } catch (IOException error) {
                buffer = null;
                errorCleanUp("Biorthogonal wavelets: Image(s) locked", true);

                return;
            }

        } // for (z = 0; z < zDim; z++)
        reconstructedImage.calcMinMax();
        
        // Compute mean square reconstruction error
        MSE = 0.0;
        for (z = 0; z < zDim; z++) {
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        		    diff = IM[y][x][z] - IMhat[y][x][z];
        		    MSE += (diff * diff);
        		}
        	}
        }
        MSE = MSE/(zDim * length);
        System.out.println("Mean square reconstruction error = " + MSE);
        Preferences.debug("Mean square reconstruction error = " + MSE + "\n", Preferences.DEBUG_ALGORITHM);
        setCompleted(true);
        return;
    }
    
    private void computeEpsilon() {
    	// epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.2204460e-16
        // epsilon is called the largest relative spacing
        epsilon = 1.0;
        double neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epsilon = neweps;
                neweps = neweps / 2.0;
            }
        } // while(true)	
    }
	
	
}
