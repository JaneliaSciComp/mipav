package gov.nih.mipav.view.renderer.WildMagic.AAM;

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
 * AAM sequence object. This is a generalization of a multi-scale model.
 * 
 * @author Ruida Cheng
 * 
 */
public class CAAMModelSeq extends CAAMModel {

	/** model vector. */
	protected Vector<CAAMModel> m_vModels = new Vector<CAAMModel>();

	/** AAM configuration vector. */
	protected Vector<String> m_vACF = new Vector<String>();

	/**
	 * Get the model at level i.
	 * 
	 * @param i
	 *            level
	 * @return corresponding model in model vetor
	 */
	public final CAAMModel Model(final int i) {
		assert (i >= 0 && i < NModels());
		return m_vModels.get(i);
	}

	/**
	 * Get the number of models.
	 * 
	 * @return number of models
	 */
	public final int NModels() {
		return m_vModels.size();
	}

	/**
	 * Get the final model
	 * 
	 * @return last element
	 */
	public final CAAMModel FinalModel() {
		return Model(NModels() - 1);
	}

	// / Returns the model reduction at that level
	public int ModelScale(int i) {
		return Model(i).ModelReduction();
	}

	/**
	 * Default multi-scale constructor.
	 */
	public CAAMModelSeq() {

	}

	/**
	 * dispose memory
	 */
	public void dispose() {

	}

	/**
	 * Wrapper to build the sequential model from the given aam configuration
	 * files.
	 * 
	 * @param SACF
	 *            aam configuration sequential file
	 * @param inDir
	 *            file dir
	 */
	public void BuildFromSACF(final String SACF, final String inDir) {
		this.BuildFromSACF(SACF, inDir, -1);
	}

	/**
	 * Driver method for model generation. This method automates the model
	 * generation as much as possible by using the various class methods for all
	 * the sequences in the task of producing a model.
	 * 
	 * @param SACF
	 *            Filename of a sequence ACF, which is a file containing a list
	 *            of acfs, one per line. E.g.
	 * 
	 *            <BOF> scale4_convex_hull.acf scale4_whiskers.acf scale4.acf
	 *            scale2.acf scale1.acf <EOF>
	 * @param inDir
	 *            Input directory where annotations (.asf) resides.
	 * @param excludeShape
	 *            Excludes one shape number 'excludeShape' from the input
	 *            directory. Default -1, i.e. no shapes are removed.
	 * 
	 *            Used to perform leave-one-out testing.
	 */
	public void BuildFromSACF(final String SACF, final String inDir,
			final int excludeShape) {

		Vector<String> asfFiles = new Vector<String>();
		asfFiles = (Vector<String>) CAAMUtil.ScanSortDir(
				CAAMUtil.AddBackSlash(inDir), "asf");
		this.BuildFromSACF(SACF, asfFiles, excludeShape);
	}

	/**
	 * Diver method for model generation.
	 * This method automates the model generation as much as possible by
	 *      using the various class methods for all the sequences in the task of
	 *      producing a model.
	 * @param SACF
	 *            Filename of a sequence ACF, which is a file containing a list
	 *            of acfs, one per line. E.g.
	 * 
	 *            <BOF> scale4_convex_hull.acf scale4_whiskers.acf scale4.acf
	 *            scale2.acf scale1.acf <EOF>
	 * @param asfFiles
	 *            Vector of asf filenames.
	 * @param excludeShape
	 *            Excludes one shape number 'excludeShape' from the input
	 *            directory. Default -1, i.e. no shapes are removed.
	 * 
	 *            Used to perform leave-one-out testing.
	 */
	public void BuildFromSACF(final String SACF, final Vector<String> asfFiles,
			final int excludeShape) {

		// delete any old models
		m_vModels.clear();

		// read acf filenames
		m_vACF = CAAMUtil.ReadLines(SACF);

		for (int i = 0; i < m_vACF.size(); i++) {

			System.err.println("\n*** Building model " + (i + 1) + "/"
					+ m_vACF.size() + " in of a sequence AAM ***");

			// build model and add to the sequence
			CAAMBuilder builder = new CAAMBuilder();
			CAAMModel model = new CAAMModel();
			final int reductionMultiplier = 1;

			builder.BuildFromFiles(model, asfFiles, m_vACF.get(i),
					reductionMultiplier, excludeShape);

			m_vModels.add(model);
		}
	}

	/**
	 * Wrapper to write the sequence AAM to disk
	 * @param filename   file name
	 * @return  success or not
	 */
	public boolean WriteModels(final String filename) {
		return WriteModels(filename, false);
	}

	/**
	 * Writes the sequence AAM to disk as a set of .txt and .amf files.
	 *      Filenames are determined from the ACF file names.
	 * @param filename
	 *            Filename of the output .samf-file.
	 * @param txt_only
	 *            If true, binary model data is not written.
	 * @return true on success, false on file errors.
	 */
	public boolean WriteModels(final String filename, final boolean txt_only) {

		boolean res = true;
		String modelname = new String();

		for (int i = 0; i < NModels() && res == true; i++) {
			modelname = filename + CAAMUtil.RemoveExt(m_vACF.get(i));
			res = Model(i).WriteModel(modelname, txt_only);
		}

		return res;
	}

	/**
	 * Reads the complete AAMModel from disk.
	 * @param filename
	 *            Input filename (.samf).
	 * @return true on success, false on file errors.
	 */
	public boolean ReadModels(final String samf) {
		int i;
		// delete any old models
		m_vModels.clear();

		final Vector<String> vAMF = CAAMUtil.ReadLines(samf);

		boolean result = true;
		for (i = 0; i < vAMF.size(); i++) {

			CAAMModel model = new CAAMModel();
			System.err.println("Reading model '" + vAMF.get(i) + "'...");
			result = model.ReadModel(vAMF.get(i));

			if (result != false) {

				m_vModels.add(model);
			}
		}
		System.err.println("\nSequence AAM containing " + vAMF.size()
				+ " models succesfully read.");

		return i > 0;
	}

	/**
	 * Scale a shape defined in 'FinalModel' coordinates to 'model'
	 *      coordinates .
	 * @param model
	 *            The model the input shape are scaled to.
	 * @param shape
	 *            Input shape, which going to be scaled.
	 */
	public void ScaleShape2Model(final int model, CAAMShape shape) {

		double ratio = (double) (this.Model(model).ModelReduction())
				/ this.FinalModel().ModelReduction();

		shape.Scale(1. / ratio);
	}

	/**
	 * Scale a shape defined in 'model' coordinates to 'FinalModel'
	 *      coordinates.
	 * @param model
	 *            The model the input shape are scaled from.
	 * @param shape
	 *            Input shape, which going to be scaled.
	 */
	public void ScaleShape2Final(final int model, CAAMShape shape) {

		double ratio = (double) (this.Model(model).ModelReduction())
				/ this.FinalModel().ModelReduction();

		shape.Scale(ratio);
	}

}