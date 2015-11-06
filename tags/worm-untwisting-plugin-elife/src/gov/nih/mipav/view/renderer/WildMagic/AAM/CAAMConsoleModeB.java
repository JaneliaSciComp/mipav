package gov.nih.mipav.view.renderer.WildMagic.AAM;

import gov.nih.mipav.model.structures.*;
import java.util.*;

/**
 * This is the Java modified version of C++ active appearance model API
 * (AAM_API). It is modified with a subset of required functions for automatic
 * MRI prostate segmentation. 
 * 
 * AAM-API LICENSE  -  file: license.txt
 * 
 * This software is freely available for non-commercial use such as
 * research and education. Please see the full disclaimer below. 
 * 
 * All publications describing work using this software should cite 
 * the reference given below. 
 * 	
 * Copyright (c) 2000-2003 Mikkel B. Stegmann, mbs@imm.dtu.dk
 * 
 * 
 * IMM, Informatics & Mathematical Modelling
 * DTU, Technical University of Denmark
 * Richard Petersens Plads, Building 321
 * DK-2800 Lyngby, Denmark
 * 
 * http://www.imm.dtu.dk/~aam/
 * 
 * 
 * 
 * REFERENCES
 * 
 * Please use the reference below, when writing articles, reports etc. where 
 * the AAM-API has been used. A draft version the article is available from 
 * the homepage. 
 * 
 * I will be happy to receive pre- or reprints of such articles.
 * 
 * /Mikkel
 * 
 * 
 * -------------
 * M. B. Stegmann, B. K. Ersboll, R. Larsen, "FAME -- A Flexible Appearance 
 * Modelling Environment", IEEE Transactions on Medical Imaging, IEEE, 2003
 * (to appear)
 * -------------
 * 
 *
 * 
 * 3RD PART SOFTWARE
 * 
 * The software is partly based on the following libraries:
 * 
 * - The Microsoft(tm) Vision Software Developers Kit, VisSDK
 * - LAPACK
 * 
 *
 * DISCLAIMER
 * 
 * This software is provided 'as-is', without any express or implied warranty.
 * In no event will the author be held liable for any damages arising from the
 * use of this software.
 * 
 * Permission is granted to anyone to use this software for any non-commercial 
 * purpose, and to alter it, subject to the following restrictions:
 * 
 * 1. The origin of this software must not be misrepresented; you must not claim
 *  that you wrote the original software. 
 *
 * 2. Altered source versions must be plainly marked as such, and must not be 
 *  misrepresented as being the original software.
 * 
 * 3. This notice may not be removed or altered from any source distribution.
 * 
 * --
 *
 * No guarantees of performance accompany this software, nor is any 
 * responsibility assumed on the part of the author or IMM. 
 * 
 * This software is provided by Mikkel B. Stegmann and IMM ``as is'' and any 
 * express or implied warranties, including, but not limited to, the implied 
 * warranties of merchantability and fitness for a particular purpose are 
 * disclaimed. In no event shall IMM or Mikkel B. Stegmann be liable for any 
 * direct, indirect, incidental, special, exemplary, or consequential damages
 * (including, but not limited to, procurement of substitute goods or services;
 * loss of use, data, or profits; or business interruption) however caused and 
 * on any theory of liability, whether in contract, strict liability, or tort 
 * (including negligence or otherwise) arising in any way out of the use of 
 * this software, even if advised of the possibility of such damage.
 * 
 * 
 * 
 *
 * $Revision: 1.4 $ 
 * $Date: 2003/04/23 14:49:15 $ 
 * 
 * 
 * @author ruida
 * 
 */
public class CAAMConsoleModeB extends CAAMConsoleMode {

	/** AAM model reference. */
	private C_AAMMODEL model;

	/**
	 * Constructor
	 * 
	 * @param progname
	 *            program name
	 */
	public CAAMConsoleModeB(final String progname) {

		m_ProgName = progname;
		m_NMinParam = 2;
		m_NMaxParam = 3;

		m_Name = "b";
		m_Usage = "<input dir> <out model> [acf file]";
		m_Desc = "Builds an Active Appearance Model.";
		m_LongDesc = "In this mode the principal component analysis, parameter optimization training\n"
				+ "etc. are done.\n\n"
				+ "input dir : Directory containing images and annotations.\n"
				+ "out model : Filename (and path) of the output model file. Ex. 'model42'.\n"
				+ "acf file  : AAM configuration file.\n";
	}

	/**
	 * Constructor
	 */
	public CAAMConsoleModeB() {

		m_ProgName = "b";
		m_NMinParam = 2;
		m_NMaxParam = 3;

		m_Name = "b";
		m_Usage = "<input dir> <out model> [acf file]";
		m_Desc = "Builds an Active Appearance Model.";
		m_LongDesc = "In this mode the principal component analysis, parameter optimization training\n"
				+ "etc. are done.\n\n"
				+ "input dir : Directory containing images and annotations.\n"
				+ "out model : Filename (and path) of the output model file. Ex. 'model42'.\n"
				+ "acf file  : AAM configuration file.\n";
	}

	/**
	 * Build the AAM model from the given image and VOIs vector
	 * 
	 * @param modelImageVector
	 *            image and VOIs vector
	 * @param modelDirectory
	 *            saved model directory
	 * @param modelName
	 *            saved model name
	 * @return nothing
	 */
	public boolean buildModel(Vector<ModelImage> modelImageVector,
			String modelDirectory, String modelName) {
		boolean success = false;
		C_AAMBUILDER builder = new C_AAMBUILDER();
		model = new C_AAMMODEL();
		System.err.println("Building Active Appearence Model.");

		// build model
		success = builder.BuildModel(model, modelImageVector);
		if (success == false)
			return false;

		// write
		if (modelName.equals("")) {
			modelName = "model";
		}

		if (model.getNumberShapeParameters() >= 2) {
			model.WriteModel(modelDirectory + modelName);
		}

		return true;
	}

	/**
	 * Build the model with the given model image and VOIs vector along
	 * 
	 * @param modelImageVector
	 *            image VOIs vector
	 * @return nothing
	 */
	public boolean buildModel(Vector<ModelImage> modelImageVector) {
		boolean success = false;
		C_AAMBUILDER builder = new C_AAMBUILDER();
		model = new C_AAMMODEL();
		System.err.println("Building Active Appearence Model.");

		// build model
		success = builder.BuildModel(model, modelImageVector);
		if (success == false)
			return false;
		return true;
	}

	/**
	 * Get the built AAM model
	 * 
	 * @return aam model
	 */
	public C_AAMMODEL getModel() {
		return model;
	}

	/**
	 * C style anchor to invoke the B console mode. Being called from the AAM
	 * console.
	 * 
	 * @param argc
	 *            number of augments
	 * @param argv
	 *            augments array
	 * @return nothing
	 */
	public int Main(int argc, String[] argv) {

		// call base implementation
		super.Main(argc, argv);

		C_AAMBUILDER builder = new C_AAMBUILDER();
		C_AAMMODEL model = new C_AAMMODEL();
		System.err.println("Building Active Appearence Model.");

		// setup input parameters
		String inDir = argv[1];
		String outModel = argv[2];
		String acf = argv[3];

		String ext = CAAMUtil.GetExt(acf);
		if (ext.equals("acf") || acf.equals("")) {

			// build model
			builder.BuildFromFiles(model, inDir, acf);

			// write
			model.WriteModel(outModel);

		} else {

			if (ext.equals("sacf")) {

				// model sequence
				CAAMModelSeq modelSeq = new CAAMModelSeq();
				modelSeq.BuildFromSACF(acf, inDir);

				// write
				modelSeq.WriteModels(outModel);

			} else {

				System.err.println("Error: unknown configuration extension.");
				System.exit(-1);
			}
		}

		// we're done
		return 0;
	}

}