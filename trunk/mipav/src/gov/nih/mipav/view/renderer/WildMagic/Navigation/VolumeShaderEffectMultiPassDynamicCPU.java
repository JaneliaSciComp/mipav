package gov.nih.mipav.view.renderer.WildMagic.Navigation;

import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarRender;
import gov.nih.mipav.view.renderer.WildMagic.Render.*;
import gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidget;
import gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidgetEffect;
import gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidgetState;
import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.Rendering.*;
import WildMagic.LibGraphics.Rendering.Texture.Type;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.renderer.WildMagic.*;
import WildMagic.LibGraphics.SceneGraph.*;

import java.util.*;

public class VolumeShaderEffectMultiPassDynamicCPU extends
		VolumeShaderEffectMultiPass {

	private Vector4f outPos;
	private Vector3f varTexCoord;
	private Vector4f fragColor;
	private Matrix4f WVPMatrix;
	// private sampler2D aSceneImage;
	private Texture aSceneImage;
	// private sampler3D bVolumeImageA;
	private Texture bVolumeImageA;
	private Vector4f BackgroundColor;
	private float StepSize;
	private float iPass;

	// private sampler3D jVolumeImageB;
	private Texture jVolumeImageB;
	private float ABBlend;

	// private sampler1D cColorMapA;
	private Texture cColorMapA;
	// private sampler1D kColorMapB;
	private Texture kColorMapB;

	// private sampler1D gOpacityMapA_GM;
	private Texture gOpacityMapA_GM;
	// private sampler1D oOpacityMapB_GM;
	private Texture oOpacityMapB_GM;

	private float Blend;

	private float clipX;
	private float clipXInv;
	private float clipY;
	private float clipYInv;
	private float clipZ;
	private float clipZInv;;

	private Vector4f clipArb;
	private Vector4f clipEye;
	private Vector4f clipEyeInv;

	// private sampler3D eNormalMapA;
	private Texture eNormalMapA;
	// private sampler3D mNormalMapB;
	private Texture mNormalMapB;

	private Vector3f MaterialEmissive;
	private Vector3f MaterialAmbient;
    private Vector4f MaterialDiffuse;
	private Vector4f MaterialSpecular;
	private Vector3f CameraModelPosition;
	private Vector3f CameraWorldPosition;;

	// private sampler3D fVolumeImageA_GM;
	private Texture fVolumeImageA_GM;
	// private sampler3D nVolumeImageB_GM;
	private Texture nVolumeImageB_GM;

	private Vector3f ColorLUTOnA;
	private Vector3f ColorLUTOnB;

	private boolean bLightsOn = false;

	protected VolumeTriPlanarRender parentScene;

	private Light[] lights;
	
	public VolumeShaderEffectMultiPassDynamicCPU(VolumeImage kVolumeImageA,
			VolumeImage kVolumeImageB, Texture kSceneTarget,
			VolumeTriPlanarRender _parentScene) {
		super(kVolumeImageA, kVolumeImageB, kSceneTarget);
		parentScene = _parentScene;
	}

	/************************************ lightingFunctions ********************************************/
	void GetDirectionalLightFactors(Vector3f kModelPosition,
			Vector3f kModelNormal, Vector3f kCameraPosition,
			Vector3f kLightDirection, float fSpecularExponent,
			float[] fDiffuseFactor, float[] fSpecularFactor) {
		float fDiff = 0.0f;
		float fSpec = 0.0f;

		// fDiff = -dot(kModelNormal,kLightDirection);
		fDiff = -kModelNormal.dot(kLightDirection);

		if (fDiff > 0.0) {
			// Vector3f kViewVector = normalize(kCameraPosition -
			// kModelPosition);
			Vector3f kViewVector = Vector3f
					.sub(kCameraPosition, kModelPosition);
			kViewVector.normalize();
			// Vector3f kHalfVector = normalize(kViewVector - kLightDirection);
			Vector3f kHalfVector = Vector3f.sub(kViewVector, kLightDirection);
			kHalfVector.normalize();
			// fSpec = dot(kModelNormal,kHalfVector);
			fSpec = kModelNormal.dot(kHalfVector);
			if (fSpec > 0.0) {
				fSpec = (float) Math.pow(fSpec, fSpecularExponent);
			} else {
				fSpec = 0.0f;
			}
		} else {
			fDiff = 0.0f;
			fSpec = 0.0f;
		}

		fDiffuseFactor[0] = fDiff;
		fSpecularFactor[0] = fSpec;
	}

	void GetPointLightFactors(Vector3f kModelPosition, Vector3f kModelNormal,
			Vector3f kCameraPosition, Vector3f kLightPosition,
			float fSpecularExponent, float[] fDiffuseFactor,
			float[] fSpecularFactor) {
		float fDiff, fSpec;

		// vec3 kVertexDirection = normalize(kModelPosition - kLightPosition);
		Vector3f kVertexDirection = Vector3f.sub(kModelPosition, kLightPosition);
		kVertexDirection.normalize();
		// fDiff = -dot(kModelNormal,kVertexDirection);
		fDiff = -kModelNormal.dot(kVertexDirection);
		if (fDiff > 0.0) {
			// vec3 kViewVector = normalize(kCameraPosition - kModelPosition);
			Vector3f kViewVector = kCameraPosition.sub(kModelPosition);
			kViewVector.normalize();
			// vec3 kHalfVector = normalize(kViewVector - kVertexDirection);
			Vector3f kHalfVector = kViewVector.sub(kVertexDirection);
			kHalfVector.normalize();
			// fSpec = dot(kModelNormal,kHalfVector);
			fSpec = kModelNormal.dot(kHalfVector);
			if (fSpec > 0.0) {
				fSpec = (float) Math.pow(fSpec, fSpecularExponent);
			} else {
				fSpec = 0.0f;
			}
		} else {
			fDiff = 0.0f;
			fSpec = 0.0f;
		}

		fDiffuseFactor[0] = fDiff;
		fSpecularFactor[0] = fSpec;
	}

	void GetSpotLightFactors(Vector3f kModelPosition, Vector3f kModelNormal,
			Vector3f kCameraPosition, Vector3f kLightPosition,
			float fSpecularExponent, Vector3f kSpotAxis, float fSpotCosAngle,
			float fSpotExponent, float[] fDiffuseFactor,
			float[] fSpecularFactor, float[] fSpotFactor) {
		float fDiff, fSpec, fSpot;

		// vec3 kVertexDirection = normalize(kModelPosition - kLightPosition);
		Vector3f kVertexDirection = Vector3f
				.sub(kModelPosition, kLightPosition);
		kVertexDirection.normalize();
		// float fVertexCosAngle = dot(kSpotAxis,kVertexDirection);
		float fVertexCosAngle = kSpotAxis.dot(kVertexDirection);
		if (fVertexCosAngle >= fSpotCosAngle) {
			// fDiff = -dot(kModelNormal,kVertexDirection);
			fDiff = -kModelNormal.dot(kVertexDirection);
			if (fDiff > 0.0) {
				// vec3 kViewVector = normalize(kCameraPosition -
				// kModelPosition);
				Vector3f kViewVector = Vector3f.sub(kCameraPosition,
						kModelPosition);
				kViewVector.normalize();
				// vec3 kHalfVector = normalize(kViewVector - kVertexDirection);
				Vector3f kHalfVector = Vector3f.sub(kViewVector,
						kVertexDirection);
				kHalfVector.normalize();
				// fSpec = dot(kModelNormal,kHalfVector);
				fSpec = kModelNormal.dot(kHalfVector);
				if (fSpec > 0.0) {
					fSpec = (float) Math.pow(fSpec, fSpecularExponent);
				} else {
					fSpec = 0.0f;
				}
				fSpot = (float) Math.pow(fVertexCosAngle, fSpotExponent);
			} else {
				fDiff = 0.0f;
				fSpec = 0.0f;
				fSpot = 0.0f;
			}
		} else {
			fDiff = 0.0f;
			fSpec = 0.0f;
			fSpot = 0.0f;
		}

		fDiffuseFactor[0] = fDiff;
		fSpecularFactor[0] = fSpec;
		fSpotFactor[0] = fSpot;
	}

	float GetAttenuation(Vector3f kModelPos, Vector3f kLightPos,
			Vector4f kAttenuation) {
		// Attenuate the color (x=constant, y=linear, z=quadratic, w=intensity).
		// Attenuation is not active when the x component is zero. The distance
		// must be computed in *world* coordinates. The distance in camera
		// coordinates is not correct when the MVP matrix has nonunit scaling
		// factors.

		// vec3 kVertexWorldDir = kModelPos - kLightPos;
		Vector3f kVertexWorldDir = Vector3f.sub(kModelPos, kLightPos);
		// vec3 kVertexModelDir = kModelPos - kLightPos;
		// vec3 kVertexWorldDir = mul(kVertexModelDir,kWMatrix);
		float fDistance = (float) Math.sqrt(kVertexWorldDir.X
				* kVertexWorldDir.X + kVertexWorldDir.Y * kVertexWorldDir.Y
				+ kVertexWorldDir.Z * kVertexWorldDir.Z);

		float fAttn = kAttenuation.W
				/ (kAttenuation.X + fDistance
						* (kAttenuation.Y + fDistance * kAttenuation.Z));

		return fAttn;
	}

	Vector4f AmbientLight(Vector3f MaterialEmissive, Vector3f MaterialAmbient,
			Vector4f LightAmbient, Vector4f LightAttenuation) {
		Vector4f kResult = new Vector4f(0.0f, 0.0f, 0.0f, 0.0f);
		// vec3 kLAmb = LightAttenuation.w*LightAmbient;
		Vector3f kLAmb = Vector3f.scale(LightAttenuation.W, LightAmbient);
		// kResult.rgb = MaterialEmissive + MaterialAmbient*kLAmb;
		kResult = Vector3f.addToVector4(MaterialEmissive,
				Vector3f.mult(kLAmb, MaterialAmbient));
		kResult.W = 1.0f;
		return kResult;
	}

	Vector4f DirectionalLight(Vector3f kModelPosition, Vector3f kModelNormal,
			Vector3f CameraWorldPosition, Vector3f MaterialEmissive,
			Vector3f MaterialAmbient, Vector3f MaterialDiffuse,
			Vector4f MaterialSpecular, Vector3f LightDirection,
			Vector3f LightAmbient, Vector3f LightDiffuse,
			Vector3f LightSpecular, Vector4f LightAttenuation)

	{
		Vector4f kResult = new Vector4f(0.0f, 0.0f, 0.0f, 0.0f);

		float[] fDiff = new float[1];
		float[] fSpec = new float[1];
		GetDirectionalLightFactors(kModelPosition, kModelNormal,
				CameraWorldPosition, LightDirection, MaterialSpecular.W, fDiff,
				fSpec);
		Vector3f kColor = Vector3f.mult(MaterialAmbient, LightAmbient);
		if (fDiff[0] > 0.0f) {
			// kColor += fDiff*MaterialDiffuse.rgb*LightDiffuse;
			kColor.add(Vector3f.scale(fDiff[0], MaterialDiffuse).mult(
					LightDiffuse));
			if (fSpec[0] > 0.0f) {
				// kColor += fSpec*MaterialSpecular.rgb*LightSpecular;
				kColor.add(Vector3f.scale(fSpec[0], MaterialSpecular).mult(
						LightSpecular));
			}
		}

		// kResult.rgb = MaterialEmissive + LightAttenuation.w*kColor;
		kResult = Vector3f.addToVector4(MaterialEmissive,
				Vector3f.scale(LightAttenuation.W, kColor));
		kResult.W = 1.0f;
		return kResult;
	}

	Vector4f PointLight(Vector3f kModelPosition, Vector3f kModelNormal,
			Vector3f CameraWorldPosition, Vector3f MaterialEmissive,
			Vector3f MaterialAmbient, Vector4f MaterialDiffuse,
			Vector4f MaterialSpecular, Vector3f LightWorldPosition,
			Vector3f LightAmbient, Vector3f LightDiffuse,
			Vector3f LightSpecular, Vector4f LightAttenuation) {
		Vector4f kResult = new Vector4f(0.0f, 0.0f, 0.0f, 0.0f);

		float[] fDiff = new float[1];
		float[] fSpec = new float[1];
		GetPointLightFactors(kModelPosition, kModelNormal, CameraWorldPosition,
				LightWorldPosition, MaterialSpecular.W, fDiff, fSpec);

		float fAttn = GetAttenuation(kModelPosition, LightWorldPosition,
				LightAttenuation);

		Vector3f kColor = Vector3f.mult(MaterialAmbient, LightAmbient);
		if (fDiff[0] > 0.0) {
			// kColor += fDiff*MaterialDiffuse.xyz*LightDiffuse;
			kColor.add(Vector3f.scale(fDiff[0], MaterialDiffuse).mult(
					LightDiffuse));
			if (fSpec[0] > 0.0) {
				// kColor += fSpec*MaterialSpecular.xyz*LightSpecular;
				kColor.add(Vector3f.scale(fSpec[0], MaterialSpecular).mult(
						LightSpecular));
			}
		}

		// kResult.rgb = MaterialEmissive + fAttn*kColor;
		kResult = Vector3f.addToVector4(MaterialEmissive,
				Vector3f.scale(fAttn, kColor));
		kResult.W = MaterialDiffuse.W;
		return kResult;
	}

	Vector4f SpotLight(Vector3f kModelPosition, Vector3f kModelNormal,
			Vector3f CameraWorldPosition, Vector3f MaterialEmissive,
			Vector3f MaterialAmbient, Vector4f MaterialDiffuse,
			Vector4f MaterialSpecular, Vector3f LightWorldPosition,
			Vector3f LightWorldDirection, Vector3f LightAmbient,
			Vector3f LightDiffuse, Vector3f LightSpecular,
			Vector4f LightSpotCutoff, Vector4f LightAttenuation) {
		Vector4f kResult = new Vector4f(0.0f, 0.0f, 0.0f, 0.0f);

		float[] fDiff = new float[1];
		float[] fSpec = new float[1];
		float[] fSpot = new float[1];
		GetSpotLightFactors(kModelPosition, kModelNormal, CameraWorldPosition,
				LightWorldPosition, MaterialSpecular.W, LightWorldDirection,
				LightSpotCutoff.Y, LightSpotCutoff.W, fDiff, fSpec, fSpot);

		float fAttn = GetAttenuation(kModelPosition, LightWorldPosition,
				LightAttenuation);

		Vector3f kColor = Vector3f.mult(MaterialAmbient, LightAmbient);
		if (fSpot[0] > 0.0f) {
			if (fDiff[0] > 0.0f) {
				// kColor += (fSpot*fDiff)*MaterialDiffuse.rgb*LightDiffuse;
				kColor.add(Vector3f.scale(fSpot[0] * fDiff[0], MaterialDiffuse)
						.mult(LightDiffuse));
				if (fSpec[0] > 0.0) {
					// kColor +=
					// (fSpot*fSpec)*MaterialSpecular.rgb*LightSpecular;
					kColor.add(Vector3f.scale(fSpot[0] * fSpec[0],
							MaterialSpecular).mult(LightSpecular));
				}
			}
		}

		// kResult.rgb = MaterialEmissive + fAttn*kColor;
		kResult = Vector3f.addToVector4(MaterialEmissive,
				Vector3f.scale(fAttn, kColor));
		kResult.W = MaterialDiffuse.W;
		return kResult;
	}

	/************************** multiHistogramFunctions *************************************/
	Vector3f computeClosestPoint(float fX, float fY, Vector4f LevLine) {
		Vector3f dir = new Vector3f(LevLine.Z - LevLine.X, LevLine.W - LevLine.Y, 0);
		dir.normalize();
		Vector3f diff = new Vector3f(fX - LevLine.X, fY - LevLine.Y, 0);
		float dot = dir.dot(diff);
		Vector3f closest = new Vector3f(dir.X * dot + LevLine.X, dir.Y * dot + LevLine.Y, 0);
		return closest;
	}

	Vector3f computeIntersect(Vector3f p0, Vector3f v0, Vector3f p1, Vector3f v1) {
		float fDet = (v1.X * v0.Y - v1.Y * v0.X);
		float len0 = v0.X * v0.X + v0.Y * v0.Y;
		float len1 = v1.X * v1.X + v1.Y * v1.Y;
		if ((fDet * fDet) < (0.00000012 * len0 * len1)) {
			return p0;
		}
		float fInvDet = 1.0f / fDet;
		Vector3f diff = new Vector3f(p1.X - p0.X, p1.Y - p0.Y, 0);
		float s = (v1.X * diff.Y - v1.Y * diff.X) * fInvDet;
		Vector3f intersectPoint = new Vector3f(v0.X * s + p0.X, v0.Y * s + p0.Y, 0);
		return intersectPoint;
	}

	float areaTwice(float ptAx, float ptAy, float ptBx, float ptBy, float ptCx, float ptCy) {
		return (((ptAx - ptCx) * (ptBy - ptCy)) - ((ptAy - ptCy) * (ptBx - ptCx)));
	}

	float computeAlphaTriangle(float fX, float fY, Vector2f fShift,
			Vector4f LevMidLine, Vector4f LevLeftLine, Vector4f LevRightLine) {
		// test that the point falls within the data bounds specified by the
		// triangle:
		float area1 = areaTwice(LevRightLine.X, LevRightLine.Y, LevRightLine.Z,
				LevRightLine.W, fX, fY);
		float area2 = areaTwice(LevRightLine.Z, LevRightLine.W, LevLeftLine.Z,
				LevLeftLine.W, fX, fY);
		float area3 = areaTwice(LevLeftLine.Z, LevLeftLine.W, LevLeftLine.X,
				LevLeftLine.Y, fX, fY);
		int inside = 0;
		if ((area1 >= 0) && (area2 >= 0) && (area3 >= 0)) {
			inside = 1;
		}
		if ((area1 <= 0) && (area2 <= 0) && (area3 <= 0)) {
			inside = 1;
		}
		if (inside == 0) {
			return 0;
		}

		float fShiftL = fShift.X;
		float fShiftR = fShift.Y;
		// Calculate where the point intersects the mid-line, using the opposite
		// edge as the direction vector:
		Vector3f perpendicDir = new Vector3f(LevLeftLine.Z - LevRightLine.Z,
				LevLeftLine.W - LevRightLine.W, 0);
		Vector3f midStart = new Vector3f(LevMidLine.X, LevMidLine.Y, 0);
		Vector3f midDir = new Vector3f(LevMidLine.Z - LevMidLine.X,
				LevMidLine.W - LevMidLine.Y, 0);
		Vector3f currentPt = new Vector3f(fX, fY, 0f);
		Vector3f closestPt = computeIntersect(currentPt, perpendicDir,
				midStart, midDir);

		// Compute the direction vector from the current point to the mid-line
		// intersection point:
		perpendicDir = new Vector3f(fX - closestPt.X, fY - closestPt.Y, 0);
		Vector3f leftStart = new Vector3f(LevLeftLine.X, LevLeftLine.Y, 0);
		Vector3f leftDir = new Vector3f(LevLeftLine.Z - LevLeftLine.X,
				LevLeftLine.W - LevLeftLine.Y, 0);
		// compute intersection with left edge:
		Vector3f leftPt = computeIntersect(closestPt, perpendicDir, leftStart,
				leftDir);
		Vector3f rightStart = new Vector3f(LevRightLine.X, LevRightLine.Y, 0);
		Vector3f rightDir = new Vector3f(LevRightLine.Z - LevRightLine.X,
				LevRightLine.W - LevRightLine.Y, 0);
		// compute intersection with right edge:
		Vector3f rightPt = computeIntersect(closestPt, perpendicDir,
				rightStart, rightDir);
		// compute distances to the edges from the mid-line intersection point
		// and the current point:
		float distMidR = (closestPt.X - rightPt.X) * (closestPt.X - rightPt.X)
				+ (closestPt.Y - rightPt.Y) * (closestPt.Y - rightPt.Y);
		float distMidL = (closestPt.X - leftPt.X) * (closestPt.X - leftPt.X)
				+ (closestPt.Y - leftPt.Y) * (closestPt.Y - leftPt.Y);
		float distMidPt = (closestPt.X - fX) * (closestPt.X - fX)
				+ (closestPt.Y - fY) * (closestPt.Y - fY);
		float distPtR = (fX - rightPt.X) * (fX - rightPt.X) + (fY - rightPt.Y)
				* (fY - rightPt.Y);
		float distPtL = (fX - leftPt.X) * (fX - leftPt.X) + (fY - leftPt.Y)
				* (fY - leftPt.Y);
		// compute alpha:
		float length = fShift.X;
		float fAlpha = 0.0f;
		if ((fX > closestPt.X) && (fX < rightPt.X)) {
			fAlpha = length + (distPtR) / (distMidR);
		} else if ((fX > closestPt.X) && (fX < leftPt.X)) {
			fAlpha = length + (distPtL) / (distMidL);
		} else if ((fX < closestPt.X) && (fX > leftPt.X)) {
			fAlpha = length + (distPtL) / (distMidL);
		} else if ((fX < closestPt.X) && (fX > rightPt.X)) {
			fAlpha = length + (distPtR) / (distMidR);
		}
		return Math.min(1, fAlpha);
	}

	float computeAlphaSquare(float fX, float fY, Vector2f fShift,
			Vector4f LevMidLine, Vector4f LevLeftLine, Vector4f LevRightLine) {
		int inside = 1;
		if ((fX < LevLeftLine.X) || (fX > LevRightLine.X)
				|| (fY < LevRightLine.Y) || (fY > LevRightLine.W)) {
			inside = 0;
		}
		if (inside == 0) {
			return 0;
		}
		float fShiftL = fShift.X;
		float fShiftR = fShift.Y;
		Vector3f closestPt = computeClosestPoint(fX, fY, LevMidLine);
		Vector3f perpendicDir = new Vector3f(fX - closestPt.X, fY - closestPt.Y, 0);
		Vector3f leftStart = new Vector3f(LevLeftLine.X, LevLeftLine.Y, 0);
		Vector3f leftDir = new Vector3f(LevLeftLine.Z - LevLeftLine.X, LevLeftLine.W - LevLeftLine.Y, 0);
		Vector3f leftPt = computeIntersect(closestPt, perpendicDir, leftStart, leftDir);
		Vector3f rightStart = new Vector3f(LevRightLine.X, LevRightLine.Y, 0);
		Vector3f rightDir = new Vector3f(LevRightLine.Z - LevRightLine.X, LevRightLine.W - LevRightLine.Y, 0);
		Vector3f rightPt = computeIntersect(closestPt, perpendicDir, rightStart, rightDir);

		float fAlpha = 0.0f;
		if ((fX > (closestPt.X - fShiftL)) && (fX < (closestPt.X + fShiftR))) {
			fAlpha = 1.0f;
		}

		if ((fX <= (closestPt.X - fShiftL)) && (fX >= leftPt.X)) {
			fAlpha = (fX - leftPt.X) / ((closestPt.X - fShiftL) - leftPt.X);
		}
		if ((fX >= (closestPt.X + fShiftR)) && (fX <= rightPt.X)) {
			fAlpha = (fX - rightPt.X) / ((closestPt.X + fShiftR) - rightPt.X);
		}
		return fAlpha;
	}

	float computeAlphaCircle(float fX, float fY, Vector4f Center,
			Vector4f MidLine, Vector4f Radius) {
		Vector2f p0 = new Vector2f();
		Vector2f p1 = new Vector2f();
		p0.X = MidLine.X - Center.X;
		p0.Y = MidLine.Y - Center.Y;
		p1.X = fX - Center.X;
		p1.Y = fY - Center.Y;
		float b = Radius.Y;
		float a = Radius.X;
		float slope = (p1.Y - p0.Y) / (p1.X - p0.X);
		float intercept = p1.Y - slope * p1.X;
		float A = b * b + a * a * slope * slope;
		float B = 2 * a * a * intercept * slope;
		float C = a * a * intercept * intercept - b * b * a * a;
		float r = B * B - 4 * A * C;
		Vector2f intersect0 = new Vector2f();
		Vector2f intersect1 = new Vector2f();
		if (r >= 0) {
			// solve for x values - using the quadratic equation
			float x3 = (-B - (float) Math.sqrt(r)) / (2f * A);
			float x4 = (-B + (float) Math.sqrt(r)) / (2f * A);
			// calculate y, since we know it's on the line at that point
			// (otherwise there would be no intersection)
			float y3 = slope * x3 + intercept;
			float y4 = slope * x4 + intercept;
			intersect0.X = Center.X + x3;
			intersect0.Y = Center.Y + y3;
			intersect1.X = Center.X + x4;
			intersect1.Y = Center.Y + y4;
			Vector2f shade = new Vector2f();
			shade.X = fX - MidLine.X;
			shade.Y = fY - MidLine.Y;
			Vector2f edge = new Vector2f();
			edge.X = intersect0.X - MidLine.X;
			edge.Y = intersect0.Y - MidLine.Y;
			if (edge.dot(shade) <= 0) {
				intersect0 = intersect1;
			}
		} else {
			float x3 = (-B - (float) Math.sqrt(r)) / (2f * A);
			float y3 = slope * x3 + intercept;
			intersect0.X = Center.X + x3;
			intersect0.Y = Center.Y + y3;
		}
		Vector2f direction = new Vector2f();
		direction.X = fX - MidLine.X;
		direction.Y = fY - MidLine.Y;
		float lengthShade = (float) Math.sqrt(direction.X * direction.X
				+ direction.Y * direction.Y);
		float diffX = intersect0.X - MidLine.X;
		float diffY = intersect0.Y - MidLine.Y;
		float length = (float) Math.sqrt(diffX * diffX + diffY * diffY);
		float fAlpha = (float) Math.max(0.0f, 1.0f - (lengthShade / length));
		return fAlpha;
	}

	// ****************************** Texture lookup
	// ******************************************

	Vector4f texture1D_colorMap(Texture texture, float position,
			boolean isColorImage) {
		Vector4f color = new Vector4f();
		GraphicsImage gImage = texture.GetImage();
		byte[] aucData = new byte[256 * 4];
		aucData = gImage.GetData();
		int r = aucData[((int) position)] & 0xff;
		int g = aucData[((int) position + 1)] & 0xff;
		int b = aucData[((int) position + 2)] & 0xff;
		int a = 1;

		// float ???????????????????????????
		color.X = (float) r / 255f;
		color.Y = (float) g / 255f;
		color.Z = (float) b / 255f;
		color.W = 1f;

		return color;
	}

	Vector4f texture1D_opacityMap(Texture texture, float position,
			boolean isColorImage) {
		Vector4f color = new Vector4f();
		GraphicsImage gImage = texture.GetImage();
		float[] afData = new float[256];
		afData = gImage.GetFloatData();

		float intensity = afData[(int) position];

		color.X = intensity / 256f;
		color.Y = intensity / 256f;
		color.Z = intensity / 256f;
		color.W = intensity / 256f;
		// ???????????????????? conversion

		return color;
	}

	Vector4f texture2D(Texture texture, Vector2f position, float value) {
		Vector4f color = new Vector4f();

		return color;
	}

	Vector4f texture3D(Texture texture, Vector3f position, boolean isColorImage) {
		Vector4f color = new Vector4f();
		GraphicsImage gImage = texture.GetImage();
		byte[] aucData = null;
		int iSize = gImage.GetQuantity();
		// if (isColorImage) {
		iSize *= 4;
		// }
		aucData = new byte[iSize];
		aucData = gImage.GetData();

		// how to interpolate

		int m_iXBound = gImage.GetBound(0);
		int m_iYBound = gImage.GetBound(1);
		int m_iZBound = gImage.GetBound(2);

		int m_iXYProduct = m_iXBound * m_iYBound;

		Vector3f imageLocation = new Vector3f();
		imageLocation.copy(position);
		imageLocation.sub(parentScene.getTranslate());
		imageLocation.X *= 1.0f / parentScene.getNormalizedXDim();
		imageLocation.Y *= 1.0f / parentScene.getNormalizedYDim();
		imageLocation.Z *= 1.0f / parentScene.getNormalizedZDim();
		imageLocation.X *= (parentScene.getImage().getExtents()[0] - 1);
		imageLocation.Y *= (parentScene.getImage().getExtents()[1] - 1);
		imageLocation.Z *= (parentScene.getImage().getExtents()[2] - 1);

		// System.err.println(" imageL.X = " + imageLocation.X + " imageL.Y = " + imageLocation.Y + " imageL.Z = " + imageLocation.Z);
		
		int iX = (int)imageLocation.X;
		int iY = (int)imageLocation.Y;
		int iZ = (int)imageLocation.Z;
		
		int iTextureIndex = iZ * m_iXYProduct + (iY * m_iXBound) + iX;
		
		int iRed = (aucData[iTextureIndex*4] & 0xff);
		int iGreen = (aucData[iTextureIndex*4+1] & 0xff);
		int iBlue =  (aucData[iTextureIndex*4+2] & 0xff);
		// System.err.println("iRed = " + iRed + "  iGreen = " + iGreen + "   iBlue = " + iBlue);
		/*
		float fX = imageLocation.X;
		int iX = (int) fX;
		float fY = imageLocation.Y;
		int iY = (int) fY;
		float fZ = imageLocation.Z;
		int iZ = (int) fZ;

		if ((iX < 0) || (iX > (m_iXBound - 1))) {
			return new Vector4f(-1, -1, -1, -1);
		}

		if ((iY < 0) || (iY > (m_iYBound - 1))) {
			return new Vector4f(-1, -1, -1, -1);
		}

		if ((iZ < 0) || (iZ > (m_iZBound - 1))) {
			return new Vector4f(-1, -1, -1, -1);
		}

		int index = iX + (m_iXBound * iY) + (m_iXYProduct * iZ);

		// compute red channel
		int i000 = index * 4;
		int i100 = i000 + 1 * 4;
		int i010 = i000 + m_iXBound * 4;
		int i110 = i100 + m_iXBound * 4;
		int i001 = i000 + m_iXYProduct * 4;
		int i101 = i100 + m_iXYProduct * 4;
		int i011 = i010 + m_iXYProduct * 4;
		int i111 = i110 + m_iXYProduct * 4;

		float fF000 = (aucData[i000] & 0x0ff);
		float fF100 = (aucData[i100] & 0x0ff);
		float fF010 = (aucData[i010] & 0x0ff);
		float fF110 = (aucData[i110] & 0x0ff);
		float fF001 = (aucData[i001] & 0x0ff);
		float fF101 = (aucData[i101] & 0x0ff);
		float fF011 = (aucData[i011] & 0x0ff);
		float fF111 = (aucData[i111] & 0x0ff);

		float fDX = fX - iX;
		float fDY = fY - iY;
		float fDZ = fZ - iZ;
		float fOmDX = 1.0f - fDX;
		float fOmDY = 1.0f - fDY;
		float fOmDZ = 1.0f - fDZ;

		float fInterp = (fOmDZ * ((fOmDY * ((fOmDX * fF000) + (fDX * fF100))) + (fDY * ((fOmDX * fF010) + (fDX * fF110)))))
				+ (fDZ * ((fOmDY * ((fOmDX * fF001) + (fDX * fF101))) + (fDY * ((fOmDX * fF011) + (fDX * fF111)))));
		int iRed = (int) (fInterp + 0.5f);

		if (iRed < 0) {
			iRed = 0;
		} else if (iRed > 255) {
			iRed = 255;
		}

		// compute green channel
		i000 = (index + 1) * 4;
		i100 = i000 + 1 * 4;
		i010 = i000 + m_iXBound * 4;
		i110 = i100 + m_iXBound * 4;
		i001 = i000 + m_iXYProduct * 4;
		i101 = i100 + m_iXYProduct * 4;
		i011 = i010 + m_iXYProduct * 4;
		i111 = i110 + m_iXYProduct * 4;

		fF000 = (aucData[i000] & 0x0ff);
		fF100 = (aucData[i100] & 0x0ff);
		fF010 = (aucData[i010] & 0x0ff);
		fF110 = (aucData[i110] & 0x0ff);
		fF001 = (aucData[i001] & 0x0ff);
		fF101 = (aucData[i101] & 0x0ff);
		fF011 = (aucData[i011] & 0x0ff);
		fF111 = (aucData[i111] & 0x0ff);

		fDX = fX - iX;
		fDY = fY - iY;
		fDZ = fZ - iZ;
		fOmDX = 1.0f - fDX;
		fOmDY = 1.0f - fDY;
		fOmDZ = 1.0f - fDZ;

		fInterp = (fOmDZ * ((fOmDY * ((fOmDX * fF000) + (fDX * fF100))) + (fDY * ((fOmDX * fF010) + (fDX * fF110)))))
				+ (fDZ * ((fOmDY * ((fOmDX * fF001) + (fDX * fF101))) + (fDY * ((fOmDX * fF011) + (fDX * fF111)))));
		int iGreen = (int) (fInterp + 0.5f);

		if (iGreen < 0) {
			iGreen = 0;
		} else if (iRed > 255) {
			iGreen = 255;
		}

		// compute blue channel
		i000 = (index + 2) * 4;
		i100 = i000 + 1 * 4;
		i010 = i000 + m_iXBound * 4;
		i110 = i100 + m_iXBound * 4;
		i001 = i000 + m_iXYProduct * 4;
		i101 = i100 + m_iXYProduct * 4;
		i011 = i010 + m_iXYProduct * 4;
		i111 = i110 + m_iXYProduct * 4;

		fF000 = (aucData[i000] & 0x0ff);
		fF100 = (aucData[i100] & 0x0ff);
		fF010 = (aucData[i010] & 0x0ff);
		fF110 = (aucData[i110] & 0x0ff);
		fF001 = (aucData[i001] & 0x0ff);
		fF101 = (aucData[i101] & 0x0ff);
		fF011 = (aucData[i011] & 0x0ff);
		fF111 = (aucData[i111] & 0x0ff);

		fDX = fX - iX;
		fDY = fY - iY;
		fDZ = fZ - iZ;
		fOmDX = 1.0f - fDX;
		fOmDY = 1.0f - fDY;
		fOmDZ = 1.0f - fDZ;

		fInterp = (fOmDZ * ((fOmDY * ((fOmDX * fF000) + (fDX * fF100))) + (fDY * ((fOmDX * fF010) + (fDX * fF110)))))
				+ (fDZ * ((fOmDY * ((fOmDX * fF001) + (fDX * fF101))) + (fDY * ((fOmDX * fF011) + (fDX * fF111)))));
		int iBlue = (int) (fInterp + 0.5f);

		if (iBlue < 0) {
			iBlue = 0;
		} else if (iBlue > 255) {
			iBlue = 255;
		}
		*/

		color.X = (float) iRed / 255f;
		color.Y = (float) iGreen / 255f;
		color.Z = (float) iBlue / 255f;
		color.W = 1.0f;

		return color;
	}

	// ******************************* createColorFunctionA
	// ************************************
	Vector4f calcColorA(Vector3f position, Vector3f model_position) {
		float localBlend = Blend;
		Vector4f color = new Vector4f(0.0f, 0.0f, 0.0f, 0.0f);

		float opacity;
		Vector4f normal = new Vector4f(0f, 0f, 0f, 0f);

		Vector4f LocalMaterialDiffuse = new Vector4f(0f, 0f, 0f, 0f);
		Vector4f LocalMaterialSpecular = new Vector4f(0f, 0f, 0f, 0f);
		Vector3f LocalMaterialAmbient = new Vector3f(0f, 0f, 0f);
		Vector3f LocalMaterialEmissive = new Vector3f(0f, 0f, 0f);
		Vector4f colorSum = new Vector4f(0f, 0f, 0f, 0f);
		Vector3f local_normal = new Vector3f(0f, 0f, 0f);

		Vector4f colorGM;
		float fMapZ;
		float multiHOpacityTemp;
		float multiHOpacitySum;
		Vector4f multiHColorSum;
		Vector4f widgetColor;

		float fMapX;
		float fMapY;

		float opacityGM = 1.0f;

		if (m_kVolumeImageA.IsColorImage()) {
			opacity = 1.0f;
			color = texture3D(bVolumeImageA, model_position, true);
		} else {
			opacity = 1.0f;
			color = texture3D(bVolumeImageA, model_position, false);
			normal = new Vector4f(color.Y, color.Z, color.W, 0);
			color = new Vector4f(color.X, color.X, color.X, color.X);
			// System.err.println("test color 1 = " + color);
		}

		if ((m_iWhichShader == SUR || m_iWhichShader == CMP_SUR) && bLightsOn) {
			if (m_kVolumeImageA.IsColorImage()) {
				normal = texture3D(eNormalMapA, model_position, true);
				normal.W = 0.0f;
			}
			LocalMaterialDiffuse = MaterialDiffuse;
			LocalMaterialSpecular = MaterialSpecular;
			LocalMaterialAmbient = MaterialAmbient;
			LocalMaterialEmissive = MaterialEmissive;
			colorSum = new Vector4f(0f, 0f, 0f, 0f);

			local_normal = Vector3f.scale(2.0f, normal.getVector3()).sub(new Vector3f(1.0f, 1.0f, 1.0f));
			local_normal.normalize();

		}

		if (m_bMultiHisto) {
			colorGM = texture3D(fVolumeImageA_GM, model_position, false);
			fMapZ = colorGM.W;
			multiHOpacityTemp = 0;
			multiHOpacitySum = 0;
			multiHColorSum = new Vector4f(0f, 0f, 0f, 0f);
			widgetColor = new Vector4f(0f, 0f, 0f, 0f);

			if (m_kVolumeImageA.IsColorImage()) {
				fMapX = color.getVector3().dot(
						new Vector3f(0.299f, 0.587f, 0.114f));
				fMapY = colorGM.getVector3().dot(
						new Vector3f(0.299f, 0.587f, 0.114f));
			} else {
				fMapX = color.X;
				fMapY = colorGM.X;
			}

			for (int i = 0; i < m_iUsedWidgets; i++) {
				if (m_akLevWidget[i].UseWidget[0] != 0f) {

					Vector2f Shift = new Vector2f(m_akLevWidget[i].Shift);
					Vector3f InvY0MY1 = new Vector3f(m_akLevWidget[i].YRatio);
					Vector4f LevColor = new Vector4f(m_akLevWidget[i].Color);
					Vector4f LevMidLine = new Vector4f(m_akLevWidget[i].MidLine);
					Vector4f LevLeftLine = new Vector4f(m_akLevWidget[i].LeftLine);
					Vector4f LevRightLine = new Vector4f(m_akLevWidget[i].RightLine);
					float BoundaryEmphasis = m_akLevWidget[i].BoundaryEmphasis[0];
					Vector4f Center = new Vector4f(m_akLevWidget[i].Center);
					Vector4f Radius = new Vector4f(m_akLevWidget[i].Radius);

					if (m_akLevWidget[i].Type == ClassificationWidgetState.Circle) {
						multiHOpacityTemp = computeAlphaCircle(fMapX, fMapY,
								Center, LevMidLine, Radius);
						widgetColor = LevColor;
					} else if (m_akLevWidget[i].Type == ClassificationWidgetState.Triangle) {
						multiHOpacityTemp = computeAlphaTriangle(fMapX, fMapY,
								Shift, LevMidLine, LevLeftLine, LevRightLine);
						widgetColor = LevColor;
					} else {
						multiHOpacityTemp = computeAlphaSquare(fMapX, fMapY,
								Shift, LevMidLine, LevLeftLine, LevRightLine);
						widgetColor = LevColor;
					}

					if (m_akLevWidget[i].UseColorMap[0] != -1f) {
						Texture kMap = VolumeTriPlanarRender.getHistogramLUTTexture((int) m_akLevWidget[i].UseColorMap[0], false);
						widgetColor = texture1D_colorMap(kMap, multiHOpacityTemp, false); // ??????? Sampler1D
						widgetColor.W = LevColor.W;
					}

					multiHOpacityTemp *= (1.0f - BoundaryEmphasis * 2.0f * (0.5f - fMapZ));
					multiHColorSum.add(Vector4f.scale(multiHOpacityTemp, widgetColor));
					multiHOpacitySum += multiHOpacityTemp;
					localBlend += (multiHOpacityTemp * LevColor.W);

					Shift = null;
					InvY0MY1 = null;
					LevColor = null;
					LevMidLine = null;
					LevLeftLine = null;
					LevRightLine = null;
					Center = null;
					Radius = null;
				}
			}

			color.copy(multiHColorSum);
			opacity = multiHOpacitySum;
			if (m_iWhichShader == SUR) {
				LocalMaterialDiffuse.copy(color);
			}

		} else {
			if (m_bGradientMag) {
				colorGM = texture3D(fVolumeImageA_GM, model_position, false);
				opacityGM = texture1D_opacityMap(gOpacityMapA_GM, colorGM.X,
						false).X;
			}
			if (m_kVolumeImageA.IsColorImage()) {
				Vector4f colorTemp = new Vector4f(0f, 0f, 0f, 0f);
				opacity = 0;
				if (ColorLUTOnA.X != 0.0) {
					colorTemp = texture1D_colorMap(cColorMapA, color.X, true);
					color.X = colorTemp.X;
					opacity += colorTemp.X;
				} else {
					color.X = 0.0f;
				}
				if (ColorLUTOnA.Y != 0.0) {
					colorTemp = texture1D_colorMap(cColorMapA, color.Y, true);
					color.Y = colorTemp.Y;
					opacity += colorTemp.W;
				} else {
					color.Y = 0.0f;
				}
				if (ColorLUTOnA.Z != 0.0) {
					colorTemp = texture1D_colorMap(cColorMapA, color.Z, true);
					color.Z = colorTemp.Z;
					opacity += colorTemp.W;
				} else {
					color.Z = 0.0f;
				}
			} else {
				color = texture1D_colorMap(cColorMapA, color.X, false);
				opacity = color.W;
			}
			if (m_bGradientMag) {
				opacity = opacity * opacityGM;
			}
		}

		if ((m_iWhichShader == SUR || m_iWhichShader == CMP_SUR) && bLightsOn) {
			if (m_iWhichShader == CMP_SUR) {
				LocalMaterialDiffuse.copy(color);
				LocalMaterialAmbient.mult(color.getVector3());
			}

			Vector4f LightAmbient = new Vector4f();
			Vector4f LightDiffuse = new Vector4f();
			Vector4f LightSpecular = new Vector4f();
			Vector4f LightSpotCutoff = new Vector4f();
			Vector4f LightAttenuation = new Vector4f();
			Vector4f LightModelPosition = new Vector4f();
			Vector4f LightModelDirection = new Vector4f();
					
			// set lights parameters
			for (int i = 0; i < m_aafLight.length; i++) {
				if (m_aafLight[i][0] != -1) {

					LightAmbient.set(lights[i].Ambient.R, lights[i].Ambient.G, lights[i].Ambient.B, 1.0f);
					LightDiffuse.set(lights[i].Diffuse.R, lights[i].Diffuse.G, lights[i].Diffuse.B, 1.0f);
					LightSpecular.set(lights[i].Specular.R, lights[i].Specular.G, lights[i].Specular.B, 1.0f);
					LightSpotCutoff.set(lights[i].Angle, lights[i].CosAngle, lights[i].SinAngle, lights[i].Exponent);
					LightAttenuation.set(lights[i].Constant, lights[i].Linear, lights[i].Quadratic, lights[i].Intensity);
					
					Transformation worldTransformation = new Transformation(parentScene.getGeometry().World);
					Vector3f kMPosition = new Vector3f(); 
					worldTransformation.ApplyInverse(lights[i].Position, kMPosition);
					LightModelPosition.set(kMPosition.X, kMPosition.Y, kMPosition.Z, 1.0f);
					Vector3f kMDVector = worldTransformation.InvertVector(lights[i].DVector);
					LightModelDirection.set(kMDVector.X, kMDVector.Y, kMDVector.Z, 0.0f);
					
					// first light is static light:
					if (i == 0) {
						switch ((int) m_aafLight[i][0]) {
						case 0: // ambient
							colorSum.add(AmbientLight(LocalMaterialEmissive,
									LocalMaterialAmbient, LightAmbient,
									LightAttenuation));
							break;
						case 1: // directional
							colorSum.add(DirectionalLight(position,
									local_normal, CameraModelPosition,
									LocalMaterialEmissive,
									LocalMaterialAmbient,
									LocalMaterialDiffuse.getVector3(),
									LocalMaterialSpecular,
									LightModelDirection.getVector3(),
									LightAmbient.getVector3(),
									LightDiffuse.getVector3(),
									LightSpecular.getVector3(),
									LightAttenuation));
							break;
						case 2: // point
							colorSum.add(PointLight(position, local_normal,
									CameraModelPosition, LocalMaterialEmissive,
									LocalMaterialAmbient, LocalMaterialDiffuse,
									LocalMaterialSpecular,
									LightModelPosition.getVector3(),
									LightAmbient.getVector3(),
									LightDiffuse.getVector3(),
									LightSpecular.getVector3(),
									LightAttenuation));
							break;
						default: // spot
							colorSum.add(SpotLight(position, local_normal,
									CameraModelPosition, LocalMaterialEmissive,
									LocalMaterialAmbient, LocalMaterialDiffuse,
									LocalMaterialSpecular,
									LightModelPosition.getVector3(),
									LightModelDirection.getVector3(),
									LightAmbient.getVector3(),
									LightDiffuse.getVector3(),
									LightSpecular.getVector3(),
									LightSpotCutoff, LightAttenuation));
							break;
						}
					} else {
						switch ((int) m_aafLight[i][0]) {
						case 0: // ambient
							colorSum.add(AmbientLight(LocalMaterialEmissive,
									LocalMaterialAmbient, LightAmbient,
									LightAttenuation));
							break;
						case 1: // directional
							colorSum.add(DirectionalLight(position,
									local_normal, CameraWorldPosition,
									LocalMaterialEmissive,
									LocalMaterialAmbient,
									LocalMaterialDiffuse.getVector3(),
									LocalMaterialSpecular,
									LightModelDirection.getVector3(),
									LightAmbient.getVector3(),
									LightDiffuse.getVector3(),
									LightSpecular.getVector3(),
									LightAttenuation));
							break;
						case 2: // point
							colorSum.add(PointLight(position, local_normal,
									CameraWorldPosition, LocalMaterialEmissive,
									LocalMaterialAmbient, LocalMaterialDiffuse,
									LocalMaterialSpecular,
									LightModelPosition.getVector3(),
									LightAmbient.getVector3(),
									LightDiffuse.getVector3(),
									LightSpecular.getVector3(),
									LightAttenuation));
							break;
						default: // spot
							colorSum.add(SpotLight(position, local_normal,
									CameraWorldPosition, LocalMaterialEmissive,
									LocalMaterialAmbient, LocalMaterialDiffuse,
									LocalMaterialSpecular,
									LightModelPosition.getVector3(),
									LightModelDirection.getVector3(),
									LightAmbient.getVector3(),
									LightDiffuse.getVector3(),
									LightSpecular.getVector3(),
									LightSpotCutoff, LightAttenuation));
							break;
						}
					}
					
				}
			}
			color.copy(colorSum);
		}
		// end color computation:
		{
			if (m_iWhichShader != MIP && m_iWhichShader != DRR) {
				opacity *= localBlend;
			} else if (m_iWhichShader == MIP || m_iWhichShader == DRR) {
				opacity = color.W;
				color.scale(localBlend * opacity);
			}
		}
		color.W = opacity;
		return color;
	}

	// ******************************* createColorFunctionB
	// ************************************
	Vector4f calcColorB(Vector3f position, Vector3f model_position) {
		float localBlend = Blend;
		// Start color computation:
		Vector4f color;
		float opacity;
		float fMapX;
		float fMapY;

		Vector4f colorSum = new Vector4f(0f, 0f, 0f, 0f);
		Vector4f LocalMaterialDiffuse = new Vector4f(0f, 0f, 0f, 0f);
		Vector4f LocalMaterialSpecular = new Vector4f(0f, 0f, 0f, 0f);
		Vector3f LocalMaterialAmbient = new Vector3f(0f, 0f, 0f);
		Vector3f LocalMaterialEmissive = new Vector3f(0f, 0f, 0f);

		Vector4f normal = new Vector4f(0f, 0f, 0f, 0f);
		float fMapZ;
		float multiHOpacityTemp;
		float multiHOpacitySum;
		Vector4f multiHColorSum;
		Vector4f widgetColor;

		Vector4f colorGM;
		float opacityGM;

		Vector3f local_normal = new Vector3f(0f, 0f, 0f);

		if (m_kVolumeImageB.IsColorImage()) {
			color = new Vector4f(0.0f, 0.0f, 0.0f, 0.0f);
			opacity = 1.0f;
			color = texture3D(jVolumeImageB, model_position, true);

		} else {
			color = new Vector4f(0.0f, 0.0f, 0.0f, 0.0f);
			opacity = 1.0f;
			color = texture3D(jVolumeImageB, model_position, false);
			normal = new Vector4f(color.Y, color.Z, color.W, 0);
			color = new Vector4f(color.X, color.X, color.X, color.X);
		}

		if ((m_iWhichShader == SUR || m_iWhichShader == CMP_SUR) && bLightsOn) {
			if (m_kVolumeImageB.IsColorImage()) {
				normal = texture3D(mNormalMapB, model_position, true);
				normal.W = 0.0f;
			}
			LocalMaterialDiffuse.copy(MaterialDiffuse);
			LocalMaterialSpecular.copy(MaterialSpecular);
			LocalMaterialAmbient.copy(MaterialAmbient);
			LocalMaterialEmissive.copy(MaterialEmissive);
			colorSum = new Vector4f(0f, 0f, 0f, 0f);

			local_normal = Vector3f.scale(2.0f, normal.getVector3()).sub(
					new Vector3f(1.0f, 1.0f, 1.0f));
			local_normal.normalize();
		}

		if (m_bMultiHisto) {
			colorGM = texture3D(nVolumeImageB_GM, model_position, false);
			fMapZ = colorGM.W;
			multiHOpacityTemp = 0;
			multiHOpacitySum = 0;
			multiHColorSum = new Vector4f(0f, 0f, 0f, 0f);
			widgetColor = new Vector4f(0f, 0f, 0f, 0f);
			if (m_kVolumeImageB.IsColorImage()) {
				fMapX = color.getVector3().dot(
						new Vector3f(0.299f, 0.587f, 0.114f));
				fMapY = colorGM.getVector3().dot(
						new Vector3f(0.299f, 0.587f, 0.114f));
			} else {
				fMapX = color.X;
				fMapY = colorGM.X;
			}

			for (int i = 0; i < m_iUsedWidgets; i++) {
				if (m_akLevWidget[i].UseWidget[0] != 0f) {
					Vector2f Shift = new Vector2f(m_akLevWidget[i].Shift);
					Vector3f InvY0MY1 = new Vector3f(m_akLevWidget[i].YRatio);
					Vector4f LevColor = new Vector4f(m_akLevWidget[i].Color);
					Vector4f LevMidLine = new Vector4f(m_akLevWidget[i].MidLine);
					Vector4f LevLeftLine = new Vector4f(
							m_akLevWidget[i].LeftLine);
					Vector4f LevRightLine = new Vector4f(
							m_akLevWidget[i].RightLine);
					float BoundaryEmphasis = m_akLevWidget[i].BoundaryEmphasis[0];
					Vector4f Center = new Vector4f(m_akLevWidget[i].Center);
					Vector4f Radius = new Vector4f(m_akLevWidget[i].Radius);

					if (m_akLevWidget[i].Type == ClassificationWidgetState.Circle) {
						multiHOpacityTemp = computeAlphaCircle(fMapX, fMapY,
								Center, LevMidLine, Radius);
						widgetColor = LevColor;
					} else if (m_akLevWidget[i].Type == ClassificationWidgetState.Triangle) {
						multiHOpacityTemp = computeAlphaTriangle(fMapX, fMapY,
								Shift, LevMidLine, LevLeftLine, LevRightLine);
						widgetColor = LevColor;
					} else {
						multiHOpacityTemp = computeAlphaSquare(fMapX, fMapY,
								Shift, LevMidLine, LevLeftLine, LevRightLine);
						widgetColor = LevColor;
					}

					if (m_akLevWidget[i].UseColorMap[0] != -1f) {
						Texture kMap = VolumeTriPlanarRender
								.getHistogramLUTTexture(
										(int) m_akLevWidget[i].UseColorMap[0],
										false);
						widgetColor = texture1D_colorMap(kMap,
								multiHOpacityTemp, false); // ??????? Sampler1D
						widgetColor.W = LevColor.W;
					}

					multiHOpacityTemp *= (1.0f - BoundaryEmphasis * 2.0f
							* (0.5f - fMapZ));
					multiHColorSum.add(Vector4f.scale(multiHOpacityTemp,
							widgetColor));
					multiHOpacitySum += multiHOpacityTemp;
					localBlend += (multiHOpacityTemp * LevColor.W);

					Shift = null;
					InvY0MY1 = null;
					LevColor = null;
					LevMidLine = null;
					LevLeftLine = null;
					LevRightLine = null;
					Center = null;
					Radius = null;
				}
			}
			color.copy(multiHColorSum);
			opacity = multiHOpacitySum;
			if (m_iWhichShader == SUR) {
				LocalMaterialDiffuse.copy(color);
			}
		} else {
			if (m_kVolumeImageB.IsColorImage()) {
				Vector4f colorTemp = new Vector4f(0f, 0f, 0f, 0f);
				opacity = 0;
				if (ColorLUTOnB.X != 0.0) {
					colorTemp = texture1D_colorMap(kColorMapB, color.X, true);
					color.X = colorTemp.X;
					opacity += colorTemp.W;
				} else {
					color.X = 0.0f;
				}
				if (ColorLUTOnB.Y != 0.0f) {
					colorTemp = texture1D_colorMap(kColorMapB, color.Y, true);
					color.Y = colorTemp.Y;
					opacity += colorTemp.W;
				} else {
					color.Y = 0.0f;
				}
				if (ColorLUTOnB.Z != 0.0) {
					colorTemp = texture1D_colorMap(kColorMapB, color.Z, true);
					color.Z = colorTemp.Z;
					opacity += colorTemp.W;
				} else {
					color.Z = 0.0f;
				}
			} else {
				color = texture1D_colorMap(kColorMapB, color.X, false);
				opacity = color.W;
			}
			if (m_bGradientMag) {
				colorGM = texture3D(nVolumeImageB_GM, model_position, false);
				opacityGM = texture1D_opacityMap(oOpacityMapB_GM, colorGM.X,
						m_kVolumeImageA.IsColorImage()).X;
				opacity = opacity * opacityGM;
			}
		}

		if ((m_iWhichShader == SUR || m_iWhichShader == CMP_SUR) && bLightsOn) {
			if (m_iWhichShader == CMP_SUR) {
				LocalMaterialDiffuse.copy(color);
				LocalMaterialAmbient.mult(color.getVector3());
			}
			
			Vector4f LightAmbient = new Vector4f();
			Vector4f LightDiffuse = new Vector4f();
			Vector4f LightSpecular = new Vector4f();
			Vector4f LightSpotCutoff = new Vector4f();
			Vector4f LightAttenuation = new Vector4f();
			Vector4f LightModelPosition = new Vector4f();
			Vector4f LightModelDirection = new Vector4f();

			for (int i = 0; i < m_aafLight.length; i++) {
				if (m_aafLight[i][0] != -1) {

					LightAmbient.set(lights[i].Ambient.R, lights[i].Ambient.G, lights[i].Ambient.B, 1.0f);
					LightDiffuse.set(lights[i].Diffuse.R, lights[i].Diffuse.G, lights[i].Diffuse.B, 1.0f);
					LightSpecular.set(lights[i].Specular.R, lights[i].Specular.G, lights[i].Specular.B, 1.0f);
					LightSpotCutoff.set(lights[i].Angle, lights[i].CosAngle, lights[i].SinAngle, lights[i].Exponent);
					LightAttenuation.set(lights[i].Constant, lights[i].Linear, lights[i].Quadratic, lights[i].Intensity);
					
					Transformation worldTransformation = new Transformation(parentScene.getGeometry().World);
					Vector3f kMPosition = new Vector3f(); 
					worldTransformation.ApplyInverse(lights[i].Position, kMPosition);
					LightModelPosition.set(kMPosition.X, kMPosition.Y, kMPosition.Z, 1.0f);
					Vector3f kMDVector = worldTransformation.InvertVector(lights[i].DVector);
					LightModelDirection.set(kMDVector.X, kMDVector.Y, kMDVector.Z, 0.0f);
					
					// first light is static light:
					if (i == 0) {
						switch ((int) m_aafLight[i][0]) {
						case 0: // ambient
							colorSum.add(AmbientLight(LocalMaterialEmissive,
									LocalMaterialAmbient, LightAmbient,
									LightAttenuation));
							break;
						case 1: // directional
							colorSum.add(DirectionalLight(position,
									local_normal, CameraModelPosition,
									LocalMaterialEmissive,
									LocalMaterialAmbient,
									LocalMaterialDiffuse.getVector3(),
									LocalMaterialSpecular,
									LightModelDirection.getVector3(),
									LightAmbient.getVector3(),
									LightDiffuse.getVector3(),
									LightSpecular.getVector3(),
									LightAttenuation));
							break;
						case 2: // point
							colorSum.add(PointLight(position, local_normal,
									CameraModelPosition, LocalMaterialEmissive,
									LocalMaterialAmbient, LocalMaterialDiffuse,
									LocalMaterialSpecular,
									LightModelPosition.getVector3(),
									LightAmbient.getVector3(),
									LightDiffuse.getVector3(),
									LightSpecular.getVector3(),
									LightAttenuation));
							break;
						default: // spot
							colorSum.add(SpotLight(position, local_normal,
									CameraModelPosition, LocalMaterialEmissive,
									LocalMaterialAmbient, LocalMaterialDiffuse,
									LocalMaterialSpecular,
									LightModelPosition.getVector3(),
									LightModelDirection.getVector3(),
									LightAmbient.getVector3(),
									LightDiffuse.getVector3(),
									LightSpecular.getVector3(),
									LightSpotCutoff, LightAttenuation));
							break;
						}
					} else {
						switch ((int) m_aafLight[i][0]) {
						case 0: // ambient
							colorSum.add(AmbientLight(LocalMaterialEmissive,
									LocalMaterialAmbient, LightAmbient,
									LightAttenuation));
							break;
						case 1: // directional
							colorSum.add(DirectionalLight(position,
									local_normal, CameraWorldPosition,
									LocalMaterialEmissive,
									LocalMaterialAmbient,
									LocalMaterialDiffuse.getVector3(),
									LocalMaterialSpecular,
									LightModelDirection.getVector3(),
									LightAmbient.getVector3(),
									LightDiffuse.getVector3(),
									LightSpecular.getVector3(),
									LightAttenuation));
							break;
						case 2: // point
							colorSum.add(PointLight(position, local_normal,
									CameraWorldPosition, LocalMaterialEmissive,
									LocalMaterialAmbient, LocalMaterialDiffuse,
									LocalMaterialSpecular,
									LightModelPosition.getVector3(),
									LightAmbient.getVector3(),
									LightDiffuse.getVector3(),
									LightSpecular.getVector3(),
									LightAttenuation));
							break;
						default: // spot
							colorSum.add(SpotLight(position, local_normal,
									CameraWorldPosition, LocalMaterialEmissive,
									LocalMaterialAmbient, LocalMaterialDiffuse,
									LocalMaterialSpecular,
									LightModelPosition.getVector3(),
									LightModelDirection.getVector3(),
									LightAmbient.getVector3(),
									LightDiffuse.getVector3(),
									LightSpecular.getVector3(),
									LightSpotCutoff, LightAttenuation));
							break;
						}
					}
				}
			}
			color.copy(colorSum);
		}
		// end color computation:

		{
			if (m_iWhichShader != MIP && m_iWhichShader != DRR) {
				opacity *= localBlend;
			} else if (m_iWhichShader == MIP || m_iWhichShader == DRR) {
				opacity = color.W;
				color.scale(localBlend * opacity);
			}
		}
		color.W = opacity;
		return color;
	}

	/**
	 * in_position in model coordinate
	 * 
	 * @param in_position
	 */
	public Vector4f p_VolumeShaderMultiPass(Vector3f in_position) {
		float bClipped = -1;
		fragColor = new Vector4f(0f, 0f, 0f, 0f);
		Vector4f colorA = new Vector4f(0f, 0f, 0f, 0f);
		Vector4f colorB = new Vector4f(0f, 0f, 0f, 0f);
		

		float[] afData = new float[16];
		parentScene.GetRenderer().SetConstantVPMatrix(0, afData);
		Matrix4f kVP = new Matrix4f(afData, true);
		// kWorld World-view-projection matrix
		WVPMatrix = Matrix4f.mult(parentScene.getSceneToWorldMatrix(), kVP);

		// model coordinate to clip coordinate ???????????????
		Vector3f position = WVPMatrix.mult(new Vector4f(in_position.X, in_position.Y, in_position.Z, 1.0f)).getVector3();

		/*
		 * Vector2f texc = ((outPos.getVector2().scale(1f/outPos.W)).add(new
		 * Vector2f(1.0f, 1.0f))).scale(0.5f); Vector3f back_position =
		 * texture2D(aSceneImage, texc, 0.0).xyz; if ( (back_position.X == 0) &&
		 * (back_position.Y == 0) && (back_position.Z == 0) ) { return; }
		 * Vector3f start = new Vector3f(); start.copy(varTexCoord); Vector3f
		 * dir = Vector3f.sub(back_position, start); dir.normalize(); float fPos
		 * = iPass; // Vector3f position = Vector3f.add(start,
		 * Vector3f.scale(fPos * StepSize, dir)); Vector3f dir2 =
		 * Vector3f.sub(position, start); dir = Vector3f.sub(back_position,
		 * start); if ( dir2.dot(dir2) > dir.dot(dir) ) { return; }
		 */
		if ((m_afDoClip[0] != 0)) {
			bClipped = 0.0f;
			if (position.X > clipX) {
				bClipped = 1.0f;
			} else if (position.X < clipXInv) {
				bClipped = 1.0f;
			} else if (position.Y > clipY) {
				bClipped = 1.0f;
			} else if (position.Y < clipYInv) {
				bClipped = 1.0f;
			} else if (position.Z > clipZ) {
				bClipped = 1.0f;
			} else if (position.Z < clipZInv) {
				bClipped = 1.0f;
			}
			if (isClipAE()) {
				if (bClipped != 1.0) {
					Vector4f aPosition = new Vector4f(0.0f, 0.0f, 0.0f, 0.0f);
					Vector3f tempSub = Vector3f.sub(position, new Vector3f(.5f, .5f, .5f));
					aPosition.X = tempSub.X;
					aPosition.Y = tempSub.Y;
					aPosition.Z = tempSub.Z;
					aPosition = WVPMatrix.mult(aPosition);
					Vector3f tempAdd = Vector3f.add(aPosition.getVector3(),
							new Vector3f(.5f, .5f, .5f));
					aPosition.X = tempAdd.X;
					aPosition.Y = tempAdd.Y;
					aPosition.Z = tempAdd.Z;
					float fDot = aPosition.getVector3().dot(
							clipEye.getVector3());
					float fDotInv = aPosition.getVector3().dot(
							clipEyeInv.getVector3());
					float fDotArb = position.dot(clipArb.getVector3());
					if ((fDot < clipEye.W) || (fDotInv > clipEyeInv.W)
							|| (fDotArb > clipArb.W)) {
						bClipped = 1.0f;
					}
				}
			}
		}
		if ((m_afDoClip[0] != 0)) {
			if (bClipped == 1.0) {
				return null;
			}
		}
		// end setup:
		// generate color function:
		if (useImageA()) {
			colorA = calcColorA(position, in_position);
		}
		if (useImageB()) {
			colorB = calcColorB(position, in_position);
		}

		if (useImageA() && !useImageB()) {
			fragColor.X = colorA.X;
			fragColor.Y = colorA.Y;
			fragColor.Z = colorA.Z;
			fragColor.W = colorA.W;
		} else if (!useImageA() && useImageB()) {
			fragColor.X = colorB.X;
			fragColor.Y = colorB.Y;
			fragColor.Z = colorB.Z;
			fragColor.W = colorB.W;
		} else if (useImageA() && useImageB()) {
			Vector3f tempFragColor = Vector3f.add(
					Vector3f.scale(ABBlend, colorA),
					Vector3f.scale((1 - ABBlend), colorB));
			fragColor.X = tempFragColor.X;
			fragColor.Y = tempFragColor.Y;
			fragColor.Z = tempFragColor.Z;

			fragColor.W = (ABBlend * colorA.W) + ((1 - ABBlend) * colorB.W);
		}
		if (fragColor.equals(new Vector4f(0f, 0f, 0f, 0f))) {
			return null;
		}
		return fragColor;
	}
	
	

	public void createProgramText() {

		boolean bAddColorMap_TexturesA = false;
		boolean bAddColorMapGM_TexturesA = false;
		boolean bAddGM_TexturesA = false;
		boolean bAddNormal_TexturesA = false;

		boolean bAddColorMap_TexturesB = false;
		boolean bAddColorMapGM_TexturesB = false;
		boolean bAddGM_TexturesB = false;
		boolean bAddNormal_TexturesB = false;

		boolean bAddWidgetColorMap_Textures = false;

		if ((m_iWhichShader == SUR || m_iWhichShader == CMP_SUR)) {
			// lighting helper functions:
			// text += lightingFunctions;
		}

		if (m_bMultiHisto) {
			// multi-histogram helper functions:
			// text += ClassificationWidgetEffect.getMultiHistogramFunctions();
		}

		// GLSL Program parameters:
		// text += basicParameters;
		if (useImageB()) {
			// text += basicParametersB;
		}

		if (!m_bMultiHisto) {
			if (useImageA()) {
				// text += colorMapA;
				bAddColorMap_TexturesA = true;
			}
			if (useImageB()) {
				// text += colorMapB;
				bAddColorMap_TexturesB = true;
			}
		}
		if (m_kVolumeImageA.IsColorImage()) {
			// text += colorParametersA;
		}
		if ((m_kVolumeImageB.GetImage() != null)
				&& m_kVolumeImageB.IsColorImage()) {
			// text += colorParametersB;
		}

		bLightsOn = false;
		if ((m_iWhichShader == SUR || m_iWhichShader == CMP_SUR)) {
			if (useImageA() && m_kVolumeImageA.IsColorImage()) {
				bAddNormal_TexturesA = true;
				// text += lightingParametersBasicColorA;
			}
			if (useImageB() && m_kVolumeImageB.IsColorImage()) {
				bAddNormal_TexturesB = true;
				// text += lightingParametersBasicColorB;
			}
			// text += lightingParametersBasic;
			// for ## Lights
			for (int i = 0; i < m_aafLight.length; i++) {
				if (m_aafLight[i][0] != -1) {
					bLightsOn = true;
					// String lightParametersSpecific =
					// lightParameters.replaceAll("#", String.valueOf(i) );
					// text += lightParametersSpecific;
				}
			}
		}

		if (m_bGradientMag || m_bMultiHisto) {
			if (useImageA()) {
				// text += gradientMagnitudeParametersA;
				bAddGM_TexturesA = true;
			}
			if (useImageB()) {
				// text += gradientMagnitudeParametersB;
				bAddGM_TexturesB = true;
			}
			if (!m_bMultiHisto) {
				if (useImageA()) {
					// text += colorMapGMA;
					bAddColorMapGM_TexturesA = true;
				}
				if (useImageB()) {
					// text += colorMapGMB;
					bAddColorMapGM_TexturesB = true;
				}
			}
		}

		if (m_bMultiHisto) {
			for (int i = 0; i < m_iUsedWidgets; i++) {
				if (m_akLevWidget[i].UseWidget[0] != 0f) {
					// text += multiHistogramWidgetParameters.replaceAll( "#",
					// String.valueOf(i) );
				}
				if (m_akLevWidget[i].UseColorMap[0] != -1f) {
					// text += multiHistogramWidgetColorParameters.replaceAll(
					// "#", String.valueOf(i) );
					bAddWidgetColorMap_Textures = true;
				}
			}
		}

		if ((m_afDoClip[0] != 0)) {
			// text += clipParameters;
		}
		if (isClipAE()) {
			// text += clipAEParameters;
		}
		// if ( (m_afBlendParam[0] != 1.0) )
		{
			// text += blendParameters;
		}
		// End Parameters

		// add generated color code:
		if (useImageA()) {
			// createColorFunctionA( bLightsOn );
		}
		if (useImageB()) {
			// createColorFunctionB( bLightsOn );
		}

		// Start code:
		// main code to compute position in volume:
		// text += mainSetup;

		if ((m_afDoClip[0] != 0)) {
			// text += clipSetup;
			if (isClipAE()) {
				// text += clipAESetup;
			}
		}
		if ((m_afDoClip[0] != 0)) {
			// text += clipEnd;
		}
		// end setup:

		// generate color function:
		if (useImageA()) {
			// text += calcColorA;
		}
		if (useImageB()) {
			// text += calcColorB;
		}

		if (useImageA() && !useImageB()) {
			// text += finalColorA;
		} else if (!useImageA() && useImageB()) {
			// text += finalColorB;
		} else if (useImageA() && useImageB()) {
			// text += finalColorAB;
		}

		// GLSL Program closing bracket:
		// text += mainEnd;
		// Done generating program text.

		// if ( (m_kPShaderCMP != null) && (m_kPShaderCMP.GetProgram() != null)
		// )
		// {
		// Add the used textures to the shader program data structures:
		int iTex = 0;
		// if ( m_kPShaderCMP != null )
		// {
		// m_kPShaderCMP.SetImageName(iTex, m_kSceneTarget.GetName(),
		// "aSceneImage");
		// m_kPShaderCMP.SetTexture(iTex++, m_kSceneTarget, "aSceneImage");
		aSceneImage = m_kSceneTarget;

		if (useImageA()) {
			// m_kPShaderCMP.SetImageName(iTex,
			// m_kVolumeImageA.GetVolumeTarget().GetName(), "bVolumeImageA" );
			// m_kPShaderCMP.SetTexture(iTex++,
			// m_kVolumeImageA.GetVolumeTarget(), "bVolumeImageA" );
			bVolumeImageA = m_kVolumeImageA.GetVolumeTarget();

		}
		// }
		if (bAddColorMap_TexturesA) {
			// m_kPShaderCMP.SetImageName(iTex,
			// m_kVolumeImageA.GetColorMapTarget().GetName(), "cColorMapA");
			// m_kPShaderCMP.SetTexture(iTex++,
			// m_kVolumeImageA.GetColorMapTarget(), "cColorMapA");
			cColorMapA = m_kVolumeImageA.GetColorMapTarget();
		}
		if (bAddNormal_TexturesA) {
			// System.err.println( iTex + " " +
			// m_kVolumeImageA.GetNormalMapTarget().GetName() );
			// m_kPShaderCMP.SetImageName(iTex,
			// m_kVolumeImageA.GetNormalMapTarget().GetName(), "eNormalMapA");
			// m_kPShaderCMP.SetTexture(iTex++,
			// m_kVolumeImageA.GetNormalMapTarget(), "eNormalMapA");
			eNormalMapA = m_kVolumeImageA.GetNormalMapTarget();
		}
		if (bAddGM_TexturesA) {
			// m_kPShaderCMP.SetImageName(iTex,
			// m_kVolumeImageA.GetGradientMapTarget().GetName(),
			// "fVolumeImageA_GM");
			// m_kPShaderCMP.SetTexture(iTex++,
			// m_kVolumeImageA.GetGradientMapTarget(), "fVolumeImageA_GM");
			fVolumeImageA_GM = m_kVolumeImageA.GetGradientMapTarget();

			if (bAddColorMapGM_TexturesA) {
				// m_kPShaderCMP.SetImageName(iTex,
				// m_kVolumeImageA.GetOpacityMapGMTarget().GetName(),
				// "gOpacityMapA_GM" );
				// m_kPShaderCMP.SetTexture(iTex++,
				// m_kVolumeImageA.GetOpacityMapGMTarget(), "gOpacityMapA_GM" );
				gOpacityMapA_GM = m_kVolumeImageA.GetOpacityMapGMTarget();
			}
		}

		// if ( bAddWidgetColorMap_Textures )
		// {
		// }

		if (useImageB()) {
			// if ( m_kPShaderCMP != null )
			// {
			// m_kPShaderCMP.SetImageName(iTex,
			// m_kVolumeImageB.GetVolumeTarget().GetName(), "jVolumeImageB" );
			// m_kPShaderCMP.SetTexture(iTex++,
			// m_kVolumeImageB.GetVolumeTarget(), "jVolumeImageB" );
			jVolumeImageB = m_kVolumeImageB.GetVolumeTarget();
			// }
			if (bAddColorMap_TexturesB) {
				// m_kPShaderCMP.SetImageName(iTex,
				// m_kVolumeImageB.GetColorMapTarget().GetName(), "kColorMapB");
				// m_kPShaderCMP.SetTexture(iTex++,
				// m_kVolumeImageB.GetColorMapTarget(), "kColorMapB");
				kColorMapB = m_kVolumeImageB.GetColorMapTarget();
			}
			if (bAddNormal_TexturesB) {
				// m_kPShaderCMP.SetImageName(iTex,
				// m_kVolumeImageB.GetNormalMapTarget().GetName(),
				// "mNormalMapB");
				// m_kPShaderCMP.SetTexture(iTex++,
				// m_kVolumeImageB.GetNormalMapTarget(), "mNormalMapB");
				mNormalMapB = m_kVolumeImageB.GetNormalMapTarget();
			}
			if (bAddGM_TexturesB) {
				// m_kPShaderCMP.SetImageName(iTex,
				// m_kVolumeImageB.GetGradientMapTarget().GetName(),
				// "nVolumeImageB_GM");
				// m_kPShaderCMP.SetTexture(iTex++,
				// m_kVolumeImageB.GetGradientMapTarget(), "nVolumeImageB_GM");
				nVolumeImageB_GM = m_kVolumeImageB.GetGradientMapTarget();
				if (bAddColorMapGM_TexturesB) {
					// m_kPShaderCMP.SetImageName(iTex,
					// m_kVolumeImageB.GetOpacityMapGMTarget().GetName(),
					// "oOpacityMapB_GM" );
					// m_kPShaderCMP.SetTexture(iTex++,
					// m_kVolumeImageB.GetOpacityMapGMTarget(),
					// "oOpacityMapB_GM" );
					oOpacityMapB_GM = m_kVolumeImageB.GetOpacityMapGMTarget();
				}
			}
		}
		/*
		 * if ( !text.equals( m_kPShaderCMP.GetProgram().GetProgramText() )) {
		 * m_kPShaderCMP.GetProgram().SetProgramText( text );
		 * //System.err.println( text ); if ( GetCProgram(0) != null ) {
		 * GetCProgram(0).Reload(true); } } return text;
		 */
		// }
		
		
		// parentScene.getOpenGLRenderer().SetGeometry(geometry);
		// set material
		// set material
	    MaterialState materialState = parentScene.getMaterialState();
		MaterialEmissive = new Vector3f(materialState.Emissive.R, materialState.Emissive.G, materialState.Emissive.B);
		MaterialAmbient = new Vector3f(materialState.Ambient.R, materialState.Ambient.G, materialState.Ambient.B);
		MaterialDiffuse = new Vector4f(materialState.Diffuse.R, materialState.Diffuse.G, materialState.Diffuse.B, 1.0f );
		MaterialSpecular = new Vector4f(materialState.Specular.R, materialState.Specular.G, materialState.Specular.B, 1.0f);
		
		// set camera Model and world positions
		Vector3f cameraLocation = parentScene.getViewCamera().GetLocation();
		CameraModelPosition = new Vector3f(cameraLocation);
		CameraWorldPosition = new Vector3f(cameraLocation);
        
		lights = parentScene.getParent().getLights();
		
		// geometry = null;
	    // parentScene.getOpenGLRenderer().SetGeometry(null);
		
	}

	private void checkPixelProgram() {
		createProgramText();
	}

	public void SetClip(int iWhich, float data, boolean bEnable) {
		super.SetClip(iWhich, data, bEnable);
		checkPixelProgram();
	}

	public void SetClipArb(float[] afEquation, boolean bEnable) {
		super.SetClipArb(afEquation, bEnable);
		checkPixelProgram();
	}

	public void SetClipEye(float[] afEquation, boolean bEnable) {
		super.SetClipEye(afEquation, bEnable);
		checkPixelProgram();
	}

	public void SetClipEyeInv(float[] afEquation, boolean bEnable) {
		super.SetClipEyeInv(afEquation, bEnable);
		checkPixelProgram();
	}

	public void SetGradientMagnitude(boolean bShow) {
		m_bGradientMag = bShow;
		checkPixelProgram();
	}

	public boolean SetLight(String kLightType, float[] afType) {
		if (super.SetLight(kLightType, afType)) {
			createProgramText();
			return true;
		}
		return false;
	}

	public void updateLevWidgetState(Vector<ClassificationWidget> kLWS) {
		if (m_iUsedWidgets != kLWS.size()) {
			for (int i = 0; i < kLWS.size(); i++) {
				m_akLevWidget[i].Copy(kLWS.elementAt(i).getState());
			}
			m_iUsedWidgets = kLWS.size();
			createProgramText();
			return;
		}
		boolean bUpdate = false;
		for (int i = 0; i < kLWS.size(); i++) {
			if ((m_akLevWidget[i].UseColorMap[0] != kLWS.elementAt(i)
					.getState().UseColorMap[0])) {
				m_akLevWidget[i].Copy(kLWS.elementAt(i).getState());
				bUpdate = true;
			}
			if ((m_akLevWidget[i].UseWidget[0] != kLWS.elementAt(i).getState().UseWidget[0])) {
				m_akLevWidget[i].Copy(kLWS.elementAt(i).getState());
				bUpdate = true;
			}
			if ((m_akLevWidget[i].InvertLUT != kLWS.elementAt(i).getState().InvertLUT)) {
				m_akLevWidget[i].Copy(kLWS.elementAt(i).getState());
				bUpdate = true;
			}
		}
		if (bUpdate) {
			createProgramText();
			return;
		}
		super.updateLevWidgetState(kLWS);
	}

	public void SURFASTMode() {
		super.SURFASTMode();
		checkPixelProgram();
	}

	public void SURMode() {
		super.SURMode();
		checkPixelProgram();
	}

	public void CMPMode() {
		super.CMPMode();
		checkPixelProgram();
	}

	public void MIPMode() {
		super.MIPMode();
		checkPixelProgram();
	}

	public void DRRMode() {
		super.DRRMode();
		checkPixelProgram();
	}

	public void MULTIHISTOMode(boolean bOn) {
		if (m_bMultiHisto == bOn) {
		 	return;
		}
		m_bMultiHisto = bOn;
		super.MULTIHISTOMode(bOn);
		checkPixelProgram();
	}

	public void SetCustomBlend(int iBlendEquation, int iLogicOp, int iSrcBlend,
			int iDstBlend, ColorRGBA kColor) {
		super.SetCustomBlend(iBlendEquation, iLogicOp, iSrcBlend, iDstBlend,
				kColor);
		checkPixelProgram();
	}

	private boolean useImageA() {
		// System.err.println( "useImageA = " + (((m_kVolumeImageA.GetImage() !=
		// null) && (m_afABBlendParam[0] != 0))) );
		return (m_kVolumeImageA.GetImage() != null);
		// return ((m_kVolumeImageA.GetImage() != null) && (m_afABBlendParam[0]
		// != 0));
	}

	private boolean useImageB() {
		// System.err.println( "useImageB = " + (((m_kVolumeImageB.GetImage() !=
		// null) && (m_afABBlendParam[0] != 1.0))) );
		return (m_kVolumeImageB.GetImage() != null);
		// return ((m_kVolumeImageB.GetImage() != null) && (m_afABBlendParam[0]
		// != 1.0));
	}
	
}