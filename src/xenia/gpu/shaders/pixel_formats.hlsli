#ifndef XENIA_GPU_D3D12_SHADERS_PIXEL_FORMATS_HLSLI_
#define XENIA_GPU_D3D12_SHADERS_PIXEL_FORMATS_HLSLI_

#define kXenosFormat_1_REVERSE 0u
#define kXenosFormat_1 1u
#define kXenosFormat_8 2u
#define kXenosFormat_1_5_5_5 3u
#define kXenosFormat_5_6_5 4u
#define kXenosFormat_6_5_5 5u
#define kXenosFormat_8_8_8_8 6u
#define kXenosFormat_2_10_10_10 7u
#define kXenosFormat_8_A 8u
#define kXenosFormat_8_B 9u
#define kXenosFormat_8_8 10u
#define kXenosFormat_Cr_Y1_Cb_Y0_REP 11u
#define kXenosFormat_Y1_Cr_Y0_Cb_REP 12u
#define kXenosFormat_16_16_EDRAM 13u
#define kXenosFormat_8_8_8_8_A 14u
#define kXenosFormat_4_4_4_4 15u
#define kXenosFormat_10_11_11 16u
#define kXenosFormat_11_11_10 17u
#define kXenosFormat_DXT1 18u
#define kXenosFormat_DXT2_3 19u
#define kXenosFormat_DXT4_5 20u
#define kXenosFormat_16_16_16_16_EDRAM 21u
#define kXenosFormat_24_8 22u
#define kXenosFormat_24_8_FLOAT 23u
#define kXenosFormat_16 24u
#define kXenosFormat_16_16 25u
#define kXenosFormat_16_16_16_16 26u
#define kXenosFormat_16_EXPAND 27u
#define kXenosFormat_16_16_EXPAND 28u
#define kXenosFormat_16_16_16_16_EXPAND 29u
#define kXenosFormat_16_FLOAT 30u
#define kXenosFormat_16_16_FLOAT 31u
#define kXenosFormat_16_16_16_16_FLOAT 32u
#define kXenosFormat_32 33u
#define kXenosFormat_32_32 34u
#define kXenosFormat_32_32_32_32 35u
#define kXenosFormat_32_FLOAT 36u
#define kXenosFormat_32_32_FLOAT 37u
#define kXenosFormat_32_32_32_32_FLOAT 38u
#define kXenosFormat_32_AS_8 39u
#define kXenosFormat_32_AS_8_8 40u
#define kXenosFormat_16_MPEG 41u
#define kXenosFormat_16_16_MPEG 42u
#define kXenosFormat_8_INTERLACED 43u
#define kXenosFormat_32_AS_8_INTERLACED 44u
#define kXenosFormat_32_AS_8_8_INTERLACED 45u
#define kXenosFormat_16_INTERLACED 46u
#define kXenosFormat_16_MPEG_INTERLACED 47u
#define kXenosFormat_16_16_MPEG_INTERLACED 48u
#define kXenosFormat_DXN 49u
#define kXenosFormat_8_8_8_8_AS_16_16_16_16 50u
#define kXenosFormat_DXT1_AS_16_16_16_16 51u
#define kXenosFormat_DXT2_3_AS_16_16_16_16 52u
#define kXenosFormat_DXT4_5_AS_16_16_16_16 53u
#define kXenosFormat_2_10_10_10_AS_16_16_16_16 54u
#define kXenosFormat_10_11_11_AS_16_16_16_16 55u
#define kXenosFormat_11_11_10_AS_16_16_16_16 56u
#define kXenosFormat_32_32_32_FLOAT 57u
#define kXenosFormat_DXT3A 58u
#define kXenosFormat_DXT5A 59u
#define kXenosFormat_CTX1 60u
#define kXenosFormat_DXT3A_AS_1_1_1_1 61u
#define kXenosFormat_8_8_8_8_GAMMA_EDRAM 62u
#define kXenosFormat_2_10_10_10_FLOAT_EDRAM 63u

#define kXenosColorRenderTargetFormat_8_8_8_8 0u
#define kXenosColorRenderTargetFormat_8_8_8_8_GAMMA 1u
#define kXenosColorRenderTargetFormat_2_10_10_10 2u
#define kXenosColorRenderTargetFormat_2_10_10_10_FLOAT 3u
#define kXenosColorRenderTargetFormat_16_16 4u
#define kXenosColorRenderTargetFormat_16_16_16_16 5u
#define kXenosColorRenderTargetFormat_16_16_FLOAT 6u
#define kXenosColorRenderTargetFormat_16_16_16_16_FLOAT 7u
#define kXenosColorRenderTargetFormat_2_10_10_10_AS_10_10_10_10 10u
#define kXenosColorRenderTargetFormat_2_10_10_10_FLOAT_AS_16_16_16_16 12u
#define kXenosColorRenderTargetFormat_32_FLOAT 14u
#define kXenosColorRenderTargetFormat_32_32_FLOAT 15u

#define kXenosDepthRenderTargetFormat_D24S8 0u
#define kXenosDepthRenderTargetFormat_D24FS8 1u

// ColorFormat packing, according to the Direct3D 11.3 functional specification.

uint XePackR8UNorm(float f) {
  return uint(saturate(f) * 255.0f + 0.5f);
}

uint XePackR5G5B5A1UNorm(float4 f) {
  uint4 n = uint4(saturate(f) * float2(31.0f, 1.0f).xxxy + 0.5f);
  return n.r | (n.g << 5) | (n.b << 10) | (n.a << 15);
}

uint XePackR5G6B5UNorm(float3 f) {
  uint3 n = uint3(saturate(f) * float3(31.0f, 63.0f, 31.0f) + 0.5f);
  return n.r | (n.g << 5) | (n.b << 11);
}

uint XePackR5G5B6UNorm(float3 f) {
  uint3 n = uint3(saturate(f) * float3(31.0f, 31.0f, 63.0f) + 0.5f);
  return n.r | (n.g << 5) | (n.b << 10);
}

uint XePackR8G8UNorm(float2 f) {
  uint2 n = uint2(saturate(f) * 255.0f + 0.5f);
  return n.r | (n.g << 8);
}

uint XePackR8G8B8UNorm(float3 f) {
  uint3 n = uint3(saturate(f) * 255.0f + 0.5f);
  return n.r | (n.g << 8) | (n.b << 16);
}

uint XePackR8G8B8A8UNorm(float4 f) {
  uint4 n = uint4(saturate(f) * 255.0f + 0.5f);
  return n.r | (n.g << 8) | (n.b << 16) | (n.a << 24);
}

uint XePackR10G10B10A2UNorm(float4 f) {
  uint4 n = uint4(saturate(f) * float2(1023.0f, 3.0f).xxxy + 0.5f);
  return n.r | (n.g << 10) | (n.b << 20) | (n.a << 30);
}

uint XePackR4G4B4A4UNorm(float4 f) {
  uint4 n = uint4(saturate(f) * 15.0f + 0.5f);
  return n.r | (n.g << 4) | (n.b << 8) | (n.a << 12);
}

uint XePackR11G11B10UNorm(float3 f) {
  uint3 n = uint3(saturate(f) * float3(2047.0f, 2047.0f, 1023.0f) + 0.5f);
  return n.r | (n.g << 11) | (n.b << 22);
}

uint XePackR10G11B11UNorm(float3 f) {
  uint3 n = uint3(saturate(f) * float3(1023.0f, 2047.0f, 2047.0f) + 0.5f);
  return n.r | (n.g << 10) | (n.b << 21);
}

uint XePackR16UNorm(float f) {
  return uint(saturate(f) * 65535.0f + 0.5f);
}

uint XePackR16G16UNorm(float2 f) {
  uint2 n = uint2(saturate(f) * 65535.0f + 0.5f);
  return n.r | (n.g << 16);
}

uint2 XePackR16G16B16A16UNorm(float4 f) {
  uint4 n = uint4(saturate(f) * 65535.0f + 0.5f);
  return n.rb | (n.ga << 16);
}

uint4 XePack16bpp4PixelsInUInt4(float4 pixel_0, float4 pixel_1, float4 pixel_2,
                                float4 pixel_3, uint format) {
  uint4 packed;
  switch (format) {
    case kXenosFormat_1_5_5_5:
      packed.x = XePackR5G5B5A1UNorm(pixel_0);
      packed.y = XePackR5G5B5A1UNorm(pixel_1);
      packed.z = XePackR5G5B5A1UNorm(pixel_2);
      packed.w = XePackR5G5B5A1UNorm(pixel_3);
      break;
    case kXenosFormat_5_6_5:
      packed.x = XePackR5G6B5UNorm(pixel_0.rgb);
      packed.y = XePackR5G6B5UNorm(pixel_1.rgb);
      packed.z = XePackR5G6B5UNorm(pixel_2.rgb);
      packed.w = XePackR5G6B5UNorm(pixel_3.rgb);
      break;
    case kXenosFormat_6_5_5:
      packed.x = XePackR5G5B6UNorm(pixel_0.rgb);
      packed.y = XePackR5G5B6UNorm(pixel_1.rgb);
      packed.z = XePackR5G5B6UNorm(pixel_2.rgb);
      packed.w = XePackR5G5B6UNorm(pixel_3.rgb);
      break;
    case kXenosFormat_8_8:
      packed.x = XePackR8G8UNorm(pixel_0.rg);
      packed.y = XePackR8G8UNorm(pixel_1.rg);
      packed.z = XePackR8G8UNorm(pixel_2.rg);
      packed.w = XePackR8G8UNorm(pixel_3.rg);
      break;
    case kXenosFormat_4_4_4_4:
      packed.x = XePackR4G4B4A4UNorm(pixel_0);
      packed.y = XePackR4G4B4A4UNorm(pixel_1);
      packed.z = XePackR4G4B4A4UNorm(pixel_2);
      packed.w = XePackR4G4B4A4UNorm(pixel_3);
      break;
    case kXenosFormat_16:
      packed.x = XePackR16UNorm(pixel_0.r);
      packed.y = XePackR16UNorm(pixel_1.r);
      packed.z = XePackR16UNorm(pixel_2.r);
      packed.w = XePackR16UNorm(pixel_3.r);
      break;
    default:
      // Treat as something (16_FLOAT).
      packed = f32tof16(float4(pixel_0.r, pixel_1.r, pixel_2.r, pixel_3.r));
      break;
  }
  return packed;
}

void XePack16bpp5PixelsInUInt41(float4 pixel_0, float4 pixel_1, float4 pixel_2,
                                float4 pixel_3, float4 pixel_4, uint format,
                                out uint4 packed, out uint packed_4) {
  switch (format) {
    case kXenosFormat_1_5_5_5:
      packed.x = XePackR5G5B5A1UNorm(pixel_0);
      packed.y = XePackR5G5B5A1UNorm(pixel_1);
      packed.z = XePackR5G5B5A1UNorm(pixel_2);
      packed.w = XePackR5G5B5A1UNorm(pixel_3);
      packed_4 = XePackR5G5B5A1UNorm(pixel_4);
      break;
    case kXenosFormat_5_6_5:
      packed.x = XePackR5G6B5UNorm(pixel_0.rgb);
      packed.y = XePackR5G6B5UNorm(pixel_1.rgb);
      packed.z = XePackR5G6B5UNorm(pixel_2.rgb);
      packed.w = XePackR5G6B5UNorm(pixel_3.rgb);
      packed_4 = XePackR5G6B5UNorm(pixel_4.rgb);
      break;
    case kXenosFormat_6_5_5:
      packed.x = XePackR5G5B6UNorm(pixel_0.rgb);
      packed.y = XePackR5G5B6UNorm(pixel_1.rgb);
      packed.z = XePackR5G5B6UNorm(pixel_2.rgb);
      packed.w = XePackR5G5B6UNorm(pixel_3.rgb);
      packed_4 = XePackR5G5B6UNorm(pixel_4.rgb);
      break;
    case kXenosFormat_8_8:
      packed.x = XePackR8G8UNorm(pixel_0.rg);
      packed.y = XePackR8G8UNorm(pixel_1.rg);
      packed.z = XePackR8G8UNorm(pixel_2.rg);
      packed.w = XePackR8G8UNorm(pixel_3.rg);
      packed_4 = XePackR8G8UNorm(pixel_4.rg);
      break;
    case kXenosFormat_4_4_4_4:
      packed.x = XePackR4G4B4A4UNorm(pixel_0);
      packed.y = XePackR4G4B4A4UNorm(pixel_1);
      packed.z = XePackR4G4B4A4UNorm(pixel_2);
      packed.w = XePackR4G4B4A4UNorm(pixel_3);
      packed_4 = XePackR4G4B4A4UNorm(pixel_4);
      break;
    case kXenosFormat_16:
      packed.x = XePackR16UNorm(pixel_0.r);
      packed.y = XePackR16UNorm(pixel_1.r);
      packed.z = XePackR16UNorm(pixel_2.r);
      packed.w = XePackR16UNorm(pixel_3.r);
      packed_4 = XePackR16UNorm(pixel_4.r);
      break;
    default:
      // Treat as something (16_FLOAT).
      packed = f32tof16(float4(pixel_0.r, pixel_1.r, pixel_2.r, pixel_3.r));
      packed_4 = f32tof16(pixel_4.r);
      break;
  }
}

uint XePack16bpp2PixelsInUInt(float4 pixel_0, float4 pixel_1, uint format) {
  uint packed;
  switch (format) {
    case kXenosFormat_1_5_5_5:
      packed = XePackR5G5B5A1UNorm(pixel_0) |
               (XePackR5G5B5A1UNorm(pixel_1) << 16u);
      break;
    case kXenosFormat_5_6_5:
      packed = XePackR5G6B5UNorm(pixel_0.rgb) |
               (XePackR5G6B5UNorm(pixel_1.rgb) << 16u);
      break;
    case kXenosFormat_6_5_5:
      packed = XePackR5G5B6UNorm(pixel_0.rgb) |
               (XePackR5G5B6UNorm(pixel_1.rgb) << 16u);
      break;
    case kXenosFormat_8_8:
      packed = XePackR8G8B8A8UNorm(float4(pixel_0.rg, pixel_1.rg));
      break;
    case kXenosFormat_4_4_4_4:
      packed = XePackR4G4B4A4UNorm(pixel_0) |
               (XePackR4G4B4A4UNorm(pixel_1) << 16u);
      break;
    case kXenosFormat_16:
      packed = XePackR16G16UNorm(float2(pixel_0.r, pixel_1.r));
      break;
    default:
      // Treat as something (16_FLOAT).
      packed = f32tof16(pixel_0.r) | (f32tof16(pixel_1.r) << 16u);
      break;
  }
  return packed;
}

uint2 XePack16bpp4PixelsInUInt2(float4 pixel_0, float4 pixel_1, float4 pixel_2,
                                float4 pixel_3, uint format) {
  uint2 packed;
  switch (format) {
    case kXenosFormat_1_5_5_5:
      packed.x = XePackR5G5B5A1UNorm(pixel_0) |
                 (XePackR5G5B5A1UNorm(pixel_1) << 16u);
      packed.y = XePackR5G5B5A1UNorm(pixel_2) |
                 (XePackR5G5B5A1UNorm(pixel_3) << 16u);
      break;
    case kXenosFormat_5_6_5:
      packed.x = XePackR5G6B5UNorm(pixel_0.rgb) |
                 (XePackR5G6B5UNorm(pixel_1.rgb) << 16u);
      packed.y = XePackR5G6B5UNorm(pixel_2.rgb) |
                 (XePackR5G6B5UNorm(pixel_3.rgb) << 16u);
      break;
    case kXenosFormat_6_5_5:
      packed.x = XePackR5G5B6UNorm(pixel_0.rgb) |
                 (XePackR5G5B6UNorm(pixel_1.rgb) << 16u);
      packed.y = XePackR5G5B6UNorm(pixel_2.rgb) |
                 (XePackR5G5B6UNorm(pixel_3.rgb) << 16u);
      break;
    case kXenosFormat_8_8:
      packed.x = XePackR8G8B8A8UNorm(float4(pixel_0.rg, pixel_1.rg));
      packed.y = XePackR8G8B8A8UNorm(float4(pixel_2.rg, pixel_3.rg));
      break;
    case kXenosFormat_4_4_4_4:
      packed.x = XePackR4G4B4A4UNorm(pixel_0) |
                 (XePackR4G4B4A4UNorm(pixel_1) << 16u);
      packed.y = XePackR4G4B4A4UNorm(pixel_2) |
                 (XePackR4G4B4A4UNorm(pixel_3) << 16u);
      break;
    case kXenosFormat_16:
      packed = XePackR16G16B16A16UNorm(float4(pixel_0.r, pixel_1.r,
                                              pixel_2.r, pixel_3.r));
      break;
    default:
      // Treat as something (16_FLOAT).
      packed = f32tof16(float2(pixel_0.r, pixel_2.r)) |
               (f32tof16(float2(pixel_1.r, pixel_3.r)) << 16u);
      break;
  }
  return packed;
}

uint2 XePack32bpp2Pixels(float4 pixel_0, float4 pixel_1, uint format) {
  uint2 packed;
  switch (format) {
    case kXenosFormat_8_8_8_8:
    // TODO(Triang3l): Investigate 8_8_8_8_A.
    case kXenosFormat_8_8_8_8_A:
    case kXenosFormat_8_8_8_8_AS_16_16_16_16:
      packed.x = XePackR8G8B8A8UNorm(pixel_0);
      packed.y = XePackR8G8B8A8UNorm(pixel_1);
      break;
    case kXenosFormat_2_10_10_10:
    case kXenosFormat_2_10_10_10_AS_16_16_16_16:
      packed.x = XePackR10G10B10A2UNorm(pixel_0);
      packed.y = XePackR10G10B10A2UNorm(pixel_1);
      break;
    case kXenosFormat_10_11_11:
    case kXenosFormat_10_11_11_AS_16_16_16_16:
      packed.x = XePackR11G11B10UNorm(pixel_0.rgb);
      packed.y = XePackR11G11B10UNorm(pixel_1.rgb);
      break;
    case kXenosFormat_11_11_10:
    case kXenosFormat_11_11_10_AS_16_16_16_16:
      packed.x = XePackR10G11B11UNorm(pixel_0.rgb);
      packed.y = XePackR10G11B11UNorm(pixel_1.rgb);
      break;
    case kXenosFormat_16_16:
      packed.x = XePackR16G16UNorm(pixel_0.rg);
      packed.y = XePackR16G16UNorm(pixel_1.rg);
      break;
    case kXenosFormat_16_16_FLOAT:
      packed = f32tof16(float2(pixel_0.r, pixel_1.r)) |
               (f32tof16(float2(pixel_0.g, pixel_1.g)) << 16u);
      break;
    default:
      // Treat as 32_FLOAT.
      packed.x = asuint(pixel_0.r);
      packed.y = asuint(pixel_1.r);
      break;
  }
  return packed;
}

uint4 XePack32bpp4Pixels(float4 pixel_0, float4 pixel_1, float4 pixel_2,
                         float4 pixel_3, uint format) {
  uint4 packed;
  switch (format) {
    case kXenosFormat_8_8_8_8:
    // TODO(Triang3l): Investigate 8_8_8_8_A.
    case kXenosFormat_8_8_8_8_A:
    case kXenosFormat_8_8_8_8_AS_16_16_16_16:
      packed.x = XePackR8G8B8A8UNorm(pixel_0);
      packed.y = XePackR8G8B8A8UNorm(pixel_1);
      packed.z = XePackR8G8B8A8UNorm(pixel_2);
      packed.w = XePackR8G8B8A8UNorm(pixel_3);
      break;
    case kXenosFormat_2_10_10_10:
    case kXenosFormat_2_10_10_10_AS_16_16_16_16:
      packed.x = XePackR10G10B10A2UNorm(pixel_0);
      packed.y = XePackR10G10B10A2UNorm(pixel_1);
      packed.z = XePackR10G10B10A2UNorm(pixel_2);
      packed.w = XePackR10G10B10A2UNorm(pixel_3);
      break;
    case kXenosFormat_10_11_11:
    case kXenosFormat_10_11_11_AS_16_16_16_16:
      packed.x = XePackR11G11B10UNorm(pixel_0.rgb);
      packed.y = XePackR11G11B10UNorm(pixel_1.rgb);
      packed.z = XePackR11G11B10UNorm(pixel_2.rgb);
      packed.w = XePackR11G11B10UNorm(pixel_3.rgb);
      break;
    case kXenosFormat_11_11_10:
    case kXenosFormat_11_11_10_AS_16_16_16_16:
      packed.x = XePackR10G11B11UNorm(pixel_0.rgb);
      packed.y = XePackR10G11B11UNorm(pixel_1.rgb);
      packed.z = XePackR10G11B11UNorm(pixel_2.rgb);
      packed.w = XePackR10G11B11UNorm(pixel_3.rgb);
      break;
    case kXenosFormat_16_16:
      packed.x = XePackR16G16UNorm(pixel_0.rg);
      packed.y = XePackR16G16UNorm(pixel_1.rg);
      packed.z = XePackR16G16UNorm(pixel_2.rg);
      packed.w = XePackR16G16UNorm(pixel_3.rg);
      break;
    case kXenosFormat_16_16_FLOAT:
      packed =
          f32tof16(float4(pixel_0.r, pixel_1.r, pixel_2.r, pixel_3.r)) |
          (f32tof16(float4(pixel_0.g, pixel_1.g, pixel_2.g, pixel_3.g)) << 16u);
      break;
    default:
      // Treat as 32_FLOAT.
      packed.x = asuint(pixel_0.r);
      packed.y = asuint(pixel_1.r);
      packed.z = asuint(pixel_2.r);
      packed.w = asuint(pixel_3.r);
      break;
  }
  return packed;
}

void XePack32bpp5Pixels(float4 pixel_0, float4 pixel_1, float4 pixel_2,
                        float4 pixel_3, float4 pixel_4, uint format,
                        out uint4 packed, out uint packed_4) {
  switch (format) {
    case kXenosFormat_8_8_8_8:
    // TODO(Triang3l): Investigate 8_8_8_8_A.
    case kXenosFormat_8_8_8_8_A:
    case kXenosFormat_8_8_8_8_AS_16_16_16_16:
      packed.x = XePackR8G8B8A8UNorm(pixel_0);
      packed.y = XePackR8G8B8A8UNorm(pixel_1);
      packed.z = XePackR8G8B8A8UNorm(pixel_2);
      packed.w = XePackR8G8B8A8UNorm(pixel_3);
      packed_4 = XePackR8G8B8A8UNorm(pixel_4);
      break;
    case kXenosFormat_2_10_10_10:
    case kXenosFormat_2_10_10_10_AS_16_16_16_16:
      packed.x = XePackR10G10B10A2UNorm(pixel_0);
      packed.y = XePackR10G10B10A2UNorm(pixel_1);
      packed.z = XePackR10G10B10A2UNorm(pixel_2);
      packed.w = XePackR10G10B10A2UNorm(pixel_3);
      packed_4 = XePackR10G10B10A2UNorm(pixel_4);
      break;
    case kXenosFormat_10_11_11:
    case kXenosFormat_10_11_11_AS_16_16_16_16:
      packed.x = XePackR11G11B10UNorm(pixel_0.rgb);
      packed.y = XePackR11G11B10UNorm(pixel_1.rgb);
      packed.z = XePackR11G11B10UNorm(pixel_2.rgb);
      packed.w = XePackR11G11B10UNorm(pixel_3.rgb);
      packed_4 = XePackR11G11B10UNorm(pixel_4.rgb);
      break;
    case kXenosFormat_11_11_10:
    case kXenosFormat_11_11_10_AS_16_16_16_16:
      packed.x = XePackR10G11B11UNorm(pixel_0.rgb);
      packed.y = XePackR10G11B11UNorm(pixel_1.rgb);
      packed.z = XePackR10G11B11UNorm(pixel_2.rgb);
      packed.w = XePackR10G11B11UNorm(pixel_3.rgb);
      packed_4 = XePackR10G11B11UNorm(pixel_4.rgb);
      break;
    case kXenosFormat_16_16:
      packed.x = XePackR16G16UNorm(pixel_0.rg);
      packed.y = XePackR16G16UNorm(pixel_1.rg);
      packed.z = XePackR16G16UNorm(pixel_2.rg);
      packed.w = XePackR16G16UNorm(pixel_3.rg);
      packed_4 = XePackR16G16UNorm(pixel_4.rg);
      break;
    case kXenosFormat_16_16_FLOAT:
      packed =
          f32tof16(float4(pixel_0.r, pixel_1.r, pixel_2.r, pixel_3.r)) |
          (f32tof16(float4(pixel_0.g, pixel_1.g, pixel_2.g, pixel_3.g)) << 16u);
      packed_4 = f32tof16(pixel_4.r) | (f32tof16(pixel_4.g) << 16u);
      break;
    default:
      // Treat as 32_FLOAT.
      packed.x = asuint(pixel_0.r);
      packed.y = asuint(pixel_1.r);
      packed.z = asuint(pixel_2.r);
      packed.w = asuint(pixel_3.r);
      packed_4 = asuint(pixel_4.r);
      break;
  }
}

uint2 XePack64bppPixel(float4 pixel, uint format) {
  uint2 packed;
  switch (format) {
    case kXenosFormat_16_16_16_16:
      packed = XePackR16G16B16A16UNorm(pixel);
      break;
    case kXenosFormat_16_16_16_16_FLOAT:
      packed = f32tof16(pixel.rb) | (f32tof16(pixel.ga) << 16u);
      break;
    default:
      // Treat as 32_32_FLOAT.
      packed = asuint(pixel.rg);
      break;
  }
  return packed;
}

void XePack64bpp4Pixels(float4 pixel_0, float4 pixel_1, float4 pixel_2,
                        float4 pixel_3, uint format, out uint4 packed_01,
                        out uint4 packed_23) {
  switch (format) {
    case kXenosFormat_16_16_16_16:
      packed_01.xy = XePackR16G16B16A16UNorm(pixel_0);
      packed_01.zw = XePackR16G16B16A16UNorm(pixel_1);
      packed_23.xy = XePackR16G16B16A16UNorm(pixel_2);
      packed_23.zw = XePackR16G16B16A16UNorm(pixel_3);
      break;
    case kXenosFormat_16_16_16_16_FLOAT:
      packed_01 =
          f32tof16(float4(pixel_0.r, pixel_0.b, pixel_1.r, pixel_1.b)) |
          (f32tof16(float4(pixel_0.g, pixel_0.a, pixel_1.g, pixel_1.a)) << 16u);
      packed_23 =
          f32tof16(float4(pixel_2.r, pixel_2.b, pixel_3.r, pixel_3.b)) |
          (f32tof16(float4(pixel_2.g, pixel_2.a, pixel_3.g, pixel_3.a)) << 16u);
      break;
    default:
      // Treat as 32_32_FLOAT.
      packed_01 = asuint(float4(pixel_0.rg, pixel_1.rg));
      packed_23 = asuint(float4(pixel_2.rg, pixel_3.rg));
      break;
  }
}

void XePack64bpp5Pixels(float4 pixel_0, float4 pixel_1, float4 pixel_2,
                        float4 pixel_3, float4 pixel_4, uint format,
                        out uint4 packed_01, out uint4 packed_23,
                        out uint2 packed_4) {
  switch (format) {
    case kXenosFormat_16_16_16_16:
      packed_01.xy = XePackR16G16B16A16UNorm(pixel_0);
      packed_01.zw = XePackR16G16B16A16UNorm(pixel_1);
      packed_23.xy = XePackR16G16B16A16UNorm(pixel_2);
      packed_23.zw = XePackR16G16B16A16UNorm(pixel_3);
      packed_4 = XePackR16G16B16A16UNorm(pixel_4);
      break;
    case kXenosFormat_16_16_16_16_FLOAT:
      packed_01 =
          f32tof16(float4(pixel_0.r, pixel_0.b, pixel_1.r, pixel_1.b)) |
          (f32tof16(float4(pixel_0.g, pixel_0.a, pixel_1.g, pixel_1.a)) << 16u);
      packed_23 =
          f32tof16(float4(pixel_2.r, pixel_2.b, pixel_3.r, pixel_3.b)) |
          (f32tof16(float4(pixel_2.g, pixel_2.a, pixel_3.g, pixel_3.a)) << 16u);
      packed_4 = f32tof16(float2(pixel_4.r, pixel_4.b)) |
                 (f32tof16(float2(pixel_4.g, pixel_4.a)) << 16u);
      break;
    default:
      // Treat as 32_32_FLOAT.
      packed_01 = asuint(float4(pixel_0.rg, pixel_1.rg));
      packed_23 = asuint(float4(pixel_2.rg, pixel_3.rg));
      packed_4 = asuint(pixel_4.rg);
      break;
  }
}

// EDRAM color format unpacking.

float XeUnpackR8UNorm(uint p) {
  return float(p & 255u) * (1.0f / 255.0f);
}

float4 XeUnpackR8UNormX4(uint4 p) {
  return float4(p & 255u) * (1.0f / 255.0f);
}

float4 XeUnpackR8G8B8A8UNorm(uint p) {
  return float4((p >> uint4(0u, 8u, 16u, 24u)) & 255u) * (1.0f / 255.0f);
}

float XeUnpackR10UNorm(uint p) {
  return float(p & 1023u) * (1.0f / 1023.0f);
}

float4 XeUnpackR10UNormX4(uint4 p) {
  return float4(p & 1023u) * (1.0f / 1023.0f);
}

float4 XeUnpackR10G10B10A2UNorm(uint p) {
  return float4((p >> uint4(0u, 10u, 20u, 30u)) & uint2(1023u, 3u).xxxy) *
         float2(1.0f / 1023.0f, 1.0f / 3.0f).xxxy;
}

float4 XeUnpackR10FloatX4(uint4 p) {
  // https://github.com/Microsoft/DirectXTex/blob/master/DirectXTex/DirectXTexConvert.cpp
  uint4 f10u32 = p & 0x3FFu;
  uint4 mantissa = f10u32 & 0x7Fu;
  uint4 exponent = f10u32 >> 7u;
  // Normalize the values for the denormalized components.
  // Exponent = 1;
  // do { Exponent--; Mantissa <<= 1; } while ((Mantissa & 0x80) == 0);
  bool4 is_denormalized = exponent == 0u;
  uint4 mantissa_lzcnt = (7u).xxxx - firstbithigh(mantissa);
  exponent = is_denormalized ? ((1u).xxxx - mantissa_lzcnt) : exponent;
  mantissa =
      is_denormalized ? ((mantissa << mantissa_lzcnt) & 0x7Fu) : mantissa;
  // Combine into 32-bit float bits and clear zeros.
  return asfloat(
      (f10u32 != 0u) ? (((exponent + 124u) << 23u) | (mantissa << 16u))
                     : (0u).xxxx);
}

float XeUnpackR10Float(uint p) {
  return XeUnpackR10FloatX4(p.xxxx).x;
}

float4 XeUnpackR10G10B10A2Float(uint p) {
  // https://github.com/Microsoft/DirectXTex/blob/master/DirectXTex/DirectXTexConvert.cpp
  uint3 rgb_f10u32 = (p.xxx >> uint3(0u, 10u, 20u)) & 0x3FFu;
  uint3 mantissa = rgb_f10u32 & 0x7Fu;
  uint3 exponent = rgb_f10u32 >> 7u;
  // Normalize the values for the denormalized components.
  // Exponent = 1;
  // do { Exponent--; Mantissa <<= 1; } while ((Mantissa & 0x80) == 0);
  bool3 is_denormalized = exponent == 0u;
  uint3 mantissa_lzcnt = (7u).xxx - firstbithigh(mantissa);
  exponent = is_denormalized ? ((1u).xxx - mantissa_lzcnt) : exponent;
  mantissa =
      is_denormalized ? ((mantissa << mantissa_lzcnt) & 0x7Fu) : mantissa;
  // Combine into 32-bit float bits and clear zeros.
  uint3 rgb_f32u32 =
      (rgb_f10u32 != 0u) ? (((exponent + 124u) << 23u) | (mantissa << 16u))
                         : (0u).xxx;
  return float4(asfloat(rgb_f32u32), float(p >> 30u) * (1.0f / 3.0f));
}

// http://web.archive.org/web/20180826210254/https://www.students.science.uu.nl/~3220516/advancedgraphics/papers/inferred_lighting.pdf
// "The format of the DSF buffer (two 16 bit channels) in EDRAM is fixed point
//  with a range of -32 to 32. The corresponding texture format is fixed point
//  with a range of 0 to 1. This requires the shader to scale the shader output
//  of 0 to 1 to -32 to 32. To maintain 16 bit precision, the texture used for
//  the resolve needs to be created with a custom format that has a range of -1
//  to 1. When sampling from this texture in a shader, the results must be
//  scaled to a 0 to 1 range."

// Upper 16 bits are ignored by XeUnpackR16Edram.

float XeUnpackR16Edram(uint p) {
  return max(float(asint(p) << 16 >> 16) * (32.0f / 32767.0f), -1.0f);
}

float4 XeUnpackR16EdramX4(uint4 p) {
  return max(float4(asint(p) << 16 >> 16) * (32.0f / 32767.0f), -1.0f);
}

float2 XeUnpackR16G16Edram(uint p) {
  return max(float2(asint(p) << int2(16, 0) >> 16) * (32.0f / 32767.0f),
             -1.0f);
}

float4 XeUnpackR16G16B16A16Edram(uint2 p) {
  return max(
      float4(asint(p).xxyy << int2(16, 0).xyxy >> 16) * (32.0f / 32767.0f),
      -1.0f);
}

// Xenos 16-bit packed textures are RGBA, but in Direct3D 12 they are BGRA.

uint4 XeR5G5B5A1ToB5G5R5A1(uint4 packed_texels) {
  return (packed_texels & 0x83E083E0u) | ((packed_texels & 0x001F001Fu) << 10) |
         ((packed_texels & 0x7C007C00u) >> 10);
}
uint2 XeR5G5B5A1ToB5G5R5A1(uint2 packed_texels) {
  return XeR5G5B5A1ToB5G5R5A1(packed_texels.xyxx).xy;
}

uint4 XeR5G6B5ToB5G6R5(uint4 packed_texels) {
  return (packed_texels & 0x07E007E0u) | ((packed_texels & 0x001F001Fu) << 11) |
         ((packed_texels & 0xF800F800u) >> 11);
}
uint2 XeR5G6B5ToB5G6R5(uint2 packed_texels) {
  return XeR5G6B5ToB5G6R5(packed_texels.xyxx).xy;
}

uint4 XeR4G4B4A4ToB4G4R4A4(uint4 packed_texels) {
  return (packed_texels & 0xF0F0F0F0u) | ((packed_texels & 0x000F000Fu) << 8) |
         ((packed_texels & 0x0F000F00u) >> 8);
}
uint2 XeR4G4B4A4ToB4G4R4A4(uint2 packed_texels) {
  return XeR4G4B4A4ToB4G4R4A4(packed_texels.xyxx).xy;
}

// RRRRR GGGGG BBBBBB to GGGGG BBBBBB RRRRR (use RBGA swizzle when reading).
uint4 XeR5G5B6ToB5G6R5WithRBGASwizzle(uint4 packed_texels) {
  return ((packed_texels & 0x001F001Fu) << 11) |
         ((packed_texels & 0xFFE0FFE0) >> 5);
}
uint2 XeR5G5B6ToB5G6R5WithRBGASwizzle(uint2 packed_texels) {
  return XeR5G5B6ToB5G6R5WithRBGASwizzle(packed_texels.xyxx).xy;
}

uint4 XeR10G11B11UNormToRGBA16(uint2 packed_texels) {
  // Red and blue.
  uint4 result =
      (((packed_texels.xxyy >> uint2(0u, 21u).xyxy) &
        uint2(1023u, 2047u).xyxy) <<
       uint2(6u, 5u).xyxy) |
      ((packed_texels.xxyy >> uint2(4u, 27u).xyxy) & uint2(63u, 31u).xyxy);
  // Green. The 5 bits to be duplicated to the bottom are already at 16.
  result.xz |= ((packed_texels & (2047u << 10u)) << (21u - 10u)) |
               (packed_texels & (31u << 16u));
  // Alpha.
  result.yw |= 0xFFFF0000u;
  return result;
}
uint2 XeR10G11B11UNormToRGBA16(uint packed_texel) {
  return XeR10G11B11UNormToRGBA16(packed_texel.xx).xy;
}
void XeR10G11B11UNormToRGBA16(uint4 packed_texels, out uint4 out_01,
                              out uint4 out_23) {
  out_01 = XeR10G11B11UNormToRGBA16(packed_texels.xy);
  out_23 = XeR10G11B11UNormToRGBA16(packed_texels.zw);
}

uint4 XeR11G11B10UNormToRGBA16(uint2 packed_texels) {
  // Red and blue.
  uint4 result =
      (((packed_texels.xxyy >> uint2(0u, 22u).xyxy) &
        uint2(2047u, 1023u).xyxy) <<
       uint2(5u, 6u).xyxy) |
      ((packed_texels.xxyy >> uint2(6u, 26u).xyxy) & uint2(31u, 63u).xyxy);
  // Green.
  result.xz |= ((packed_texels & (2047u << 11u)) << (21u - 11u)) |
               ((packed_texels & (31u << 17u)) >> (17u - 16u));
  // Alpha.
  result.yw |= 0xFFFF0000u;
  return result;
}
uint2 XeR11G11B10UNormToRGBA16(uint packed_texel) {
  return XeR11G11B10UNormToRGBA16(packed_texel.xx).xy;
}
void XeR11G11B10UNormToRGBA16(uint4 packed_texels, out uint4 out_01,
                              out uint4 out_23) {
  out_01 = XeR11G11B10UNormToRGBA16(packed_texels.xy);
  out_23 = XeR11G11B10UNormToRGBA16(packed_texels.zw);
}

// Assuming the original number has only 10 bits.
uint2 XeSNorm10To16(uint2 s10) {
  uint2 signs = s10 >> 9u;
  // -512 and -511 are both -1.0, but with -512 the conversion will overflow.
  s10 = s10 == 0x200u ? 0x201u : s10;
  // Take the absolute value.
  s10 = (s10 ^ (signs ? 0x3FFu : 0u)) + signs;
  // Expand the 9-bit absolute value to 15 bits like unorm.
  s10 = (s10 << 6u) | (s10 >> 3u);
  // Apply the sign.
  return (s10 ^ (signs ? 0xFFFFu : 0u)) + signs;
}

// Assuming the original number has only 11 bits.
uint2 XeSNorm11To16(uint2 s11) {
  uint2 signs = s11 >> 10u;
  // -1024 and -1023 are both -1.0, but with -1024 the conversion will overflow.
  s11 = s11 == 0x400u ? 0x401u : s11;
  // Take the absolute value.
  s11 = (s11 ^ (signs ? 0x7FFu : 0u)) + signs;
  // Expand the 10-bit absolute value to 15 bits like unorm.
  s11 = (s11 << 5u) | (s11 >> 5u);
  // Apply the sign.
  return (s11 ^ (signs ? 0xFFFFu : 0u)) + signs;
}

uint4 XeR10G11B11SNormToRGBA16(uint2 packed_texels) {
  // uint4(RG0, RG1, BA0, BA1).xzyw == uint4(RG0, BA0, RG1, BA1).
  return uint4(XeSNorm10To16(packed_texels & 1023u) |
                   (XeSNorm11To16((packed_texels >> 10u) & 2047u) << 16u),
               XeSNorm11To16(packed_texels >> 21u) | 0x7FFF0000u).xzyw;
}
uint2 XeR10G11B11SNormToRGBA16(uint packed_texel) {
  return XeR10G11B11SNormToRGBA16(packed_texel.xx).xy;
}
void XeR10G11B11SNormToRGBA16(uint4 packed_texels, out uint4 out_01,
                              out uint4 out_23) {
  out_01 = XeR10G11B11SNormToRGBA16(packed_texels.xy);
  out_23 = XeR10G11B11SNormToRGBA16(packed_texels.zw);
}

uint4 XeR11G11B10SNormToRGBA16(uint2 packed_texels) {
  // uint4(RG0, RG1, BA0, BA1).xzyw == uint4(RG0, BA0, RG1, BA1).
  return uint4(XeSNorm11To16(packed_texels & 2047u) |
                   (XeSNorm11To16((packed_texels >> 11u) & 2047u) << 16u),
               XeSNorm10To16(packed_texels >> 22u) | 0x7FFF0000u).xzyw;
}
uint2 XeR11G11B10SNormToRGBA16(uint packed_texel) {
  return XeR11G11B10SNormToRGBA16(packed_texel.xx).xy;
}
void XeR11G11B10SNormToRGBA16(uint4 packed_texels, out uint4 out_01,
                              out uint4 out_23) {
  out_01 = XeR11G11B10SNormToRGBA16(packed_texels.xy);
  out_23 = XeR11G11B10SNormToRGBA16(packed_texels.zw);
}

// Based on CFloat24 from d3dref9.dll and the 6e4 code from:
// https://github.com/Microsoft/DirectXTex/blob/master/DirectXTex/DirectXTexConvert.cpp
// 6e4 has a different exponent bias allowing [0,512) values, 20e4 allows [0,2).
// We also can't clamp the stored value to 1 as load->store->load must be exact.

uint XeFloat32To20e4(uint f32u32) {
  // Keep only positive (high bit set means negative for both float and int) and
  // saturate to the maximum representable value near 2 (also dropping NaNs).
  f32u32 = min((f32u32 <= 0x7FFFFFFFu) ? f32u32 : 0u, 0x3FFFFFF8u);
  uint denormalized =
      ((f32u32 & 0x7FFFFFu) | 0x800000u) >> min(113u - (f32u32 >> 23u), 24u);
  uint f24u32 = (f32u32 < 0x38800000u) ? denormalized : (f32u32 + 0xC8000000u);
  return ((f24u32 + 3u + ((f24u32 >> 3u) & 1u)) >> 3u) & 0xFFFFFFu;
}

uint XeFloat20e4To32(uint f24u32, bool remap_to_0_to_0_5 = false) {
  uint mantissa = f24u32 & 0xFFFFFu;
  uint exponent = f24u32 >> 20u;
  // Normalize the values for the denormalized components.
  // Exponent = 1;
  // do { Exponent--; Mantissa <<= 1; } while ((Mantissa & 0x100000) == 0);
  bool is_denormalized = exponent == 0u;
  uint mantissa_lzcnt = 20u - firstbithigh(mantissa);
  exponent = is_denormalized ? (1u - mantissa_lzcnt) : exponent;
  mantissa =
      is_denormalized ? ((mantissa << mantissa_lzcnt) & 0xFFFFFu) : mantissa;
  // Combine into 32-bit float bits and clear zeros and, if needed, bias the
  // exponent.
  return (f24u32 != 0u)
             ? ((exponent + (112u - uint(remap_to_0_to_0_5))) << 23u) |
                (mantissa << 3u)
             : 0u;
}

uint4 XeFloat20e4To32(uint4 f24u32) {
  uint4 mantissa = f24u32 & 0xFFFFFu;
  uint4 exponent = f24u32 >> 20u;
  // Normalize the values for the denormalized components.
  // Exponent = 1;
  // do { Exponent--; Mantissa <<= 1; } while ((Mantissa & 0x100000) == 0);
  bool4 is_denormalized = exponent == 0u;
  uint4 mantissa_lzcnt = (20u).xxxx - firstbithigh(mantissa);
  exponent = is_denormalized ? ((1u).xxxx - mantissa_lzcnt) : exponent;
  mantissa =
      is_denormalized ? ((mantissa << mantissa_lzcnt) & 0xFFFFFu) : mantissa;
  // Combine into 32-bit float bits and clear zeros.
  return (f24u32 != 0u) ? (((exponent + 112u) << 23u) | (mantissa << 3u))
                        : (0u).xxxx;
}

uint2 XeFloat20e4To32(uint2 f24u32) {
  return XeFloat20e4To32(f24u32.xyxx).xy;
}

float4 XeUNorm24To32(uint4 n24) {
  // Not 1.0f / 16777215.0f as that gives an incorrect result (like for a very
  // common 0xC00000 which clears 2_10_10_10 to 0001). Division by 2^24 is just
  // an exponent shift though, thus exact.
  // Division by 16777215.0f behaves this way.
  return float4(n24 + (n24 >> 23u)) * (1.0f / 16777216.0f);
}

float2 XeUNorm24To32(uint2 n24) {
  return XeUNorm24To32(n24.xyxx).xy;
}

// Converts endpoint BGR (first - X of the return value - in the low 16 bits,
// second - Y of the return value - in the high) of a DXT blocks to 8-bit, with
// 2 unused bits between each component to allow for overflow when multiplying
// by values up to 3 (so multiplication can be done for all components at once).
// Relative ordering between endpoints is preserved, so result.x > result.y
// (color0 > color1) and result.x <= result.y (color0 <= color1) can be used for
// choosing the DXT1 mode.
uint2 XeDXTColorEndpointsToBGR8In10(uint bgr_end_565) {
  // Converting 5:6:5 to 8:8:8 similar to how Compressonator does that.
  // https://github.com/GPUOpen-Tools/compressonator/blob/master/CMP_CompressonatorLib/DXTC/Codec_DXTC_RGBA.cpp#L340
  uint2 bgr_end_8in10 =
      // Blue in 0:4 and 16:20 - to 3:7.
      (uint2(bgr_end_565 << 3u, bgr_end_565 >> (16u - 3u)) & (31u << 3u)) |
      // Green in 5:10 and 21:26 - to 12:17.
      (uint2(bgr_end_565 << (12u - 5u), bgr_end_565 >> (21u - 12u)) &
       (63u << 12u)) |
      // Red in 11:15 and 27:31 - to 23:27.
      (uint2(bgr_end_565 << (23u - 11u), bgr_end_565 >> (27u - 23u)) &
       (31u << 23u));
   // Apply the lower bit replication to give full dynamic range.
   // Blue and red.
   bgr_end_8in10 |= (bgr_end_8in10 >> 5u) & (7u | (7u << 20u));
   // Green.
   bgr_end_8in10 |= (bgr_end_8in10 >> 6u) & (3u << 10u);
   return bgr_end_8in10;
}

// Sorts the color indices of a DXT3/DXT5 or a DXT1 opaque block so they can be
// used as the weights for the second endpoint, from 0 to 3. To get the weights
// for the first endpoint, apply bitwise NOT to the result.
uint4 XeDXTHighColorWeights(uint4 codes) {
  // Initially 00 = 3:0, 01 = 0:3, 10 = 2:1, 11 = 1:2.
  // Swap bits. 00 = 3:0, 01 = 2:1, 10 = 0:3, 11 = 1:2.
  codes = ((codes & 0x55555555u) << 1u) | ((codes & 0xAAAAAAAAu) >> 1u);
  // Swap 10 and 11. 00 = 3:0, 01 = 2:1, 10 = 1:2, 11 = 0:3.
  return codes ^ ((codes & 0xAAAAAAAAu) >> 1u);
}
uint2 XeDXTHighColorWeights(uint2 codes) {
  return XeDXTHighColorWeights(codes.xyxx).xy;
}
uint XeDXTHighColorWeights(uint codes) {
  return XeDXTHighColorWeights(codes.xx).x;
}

// Get the RGB colors of one row of a DXT opaque block. Endpoint colors can be
// obtained using XeDXTColorEndpointsToBGR8In10 (8 bits with 2 bits of free
// space between each), weights can be obtained using XeDXTHighColorWeights.
// Alpha is set to 0 in the result. Weights must be shifted right by 8 * row
// index before calling.
uint4 XeDXTOpaqueRowToRGB8(uint2 bgr_end_8in10, uint weights_high) {
  const uint4 weights_shifts = uint4(0u, 2u, 4u, 6u);
  uint4 bgr_row_8in10_3x =
      (((~weights_high).xxxx >> weights_shifts) & 3u) * bgr_end_8in10.x +
      ((weights_high.xxxx >> weights_shifts) & 3u) * bgr_end_8in10.y;
  return (((bgr_row_8in10_3x & 1023u) / 3u) << 16u) |
         ((((bgr_row_8in10_3x >> 10u) & 1023u) / 3u) << 8u) |
         ((bgr_row_8in10_3x >> 20u) / 3u);
}

// Sort the color indices of four transparent DXT1 blocks so bits of them can be
// used as endpoint weights (lower bit for the low endpoint, upper bit for the
// high endpoint, and both bits for 1/2 of each, AND of those bits can be used
// as the right shift amount for mixing the two colors in the punchthrough
// mode). Zero for the punchthrough alpha texels.
uint4 XeDXT1TransWeights(uint4 codes) {
  // Initially 00 = 1:0, 01 = 0:1, 10 = 1:1, 11 = 0:0.
  // 00 = 0:0, 01 = 1:1, 10 = 0:1, 11 = 1:0.
  codes = ~codes;
  // 00 = 0:0, 01 = 1:0, 10 = 0:1, 11 = 1:1.
  return codes ^ ((codes & 0x55555555u) << 1u);
}

// Gets the RGBA colors of one row of a DXT1 punchthrough block. Endpoint colors
// can be obtained using XeDXTColorEndpointsToBGR8In10 (8 bits with 2 bits of
// free space between each), weights can be obtained using XeDXT1TransWeights
// and must be shifted right by 8 * row index before calling.
uint4 XeDXT1TransRowToRGBA8(uint2 bgr_end_8in10, uint weights) {
  const uint4 weights_shifts_low = uint4(0u, 2u, 4u, 6u);
  const uint4 weights_shifts_high = uint4(1u, 3u, 5u, 7u);
  uint4 bgr_row_8in10_scaled =
      ((weights.xxxx >> weights_shifts_low) & 1u) * bgr_end_8in10.x +
      ((weights.xxxx >> weights_shifts_high) & 1u) * bgr_end_8in10.y;
  // Whether the texel is (RGB0+RGB1)/2 - divide the weighted sum by 2 (shift
  // right by 1) if it is.
  uint4 weights_sums_log2 = weights & ((weights & 0xAAAAAAAAu) >> 1u);
  uint4 bgr_shift = (weights_sums_log2.xxxx >> weights_shifts_low) & 1u;
  // Whether the texel is opaque.
  uint4 weights_alpha =
      (weights & 0x55555555u) | ((weights & 0xAAAAAAAAu) >> 1u);
  return (((bgr_row_8in10_scaled & 1023u) >> bgr_shift) << 16u) +
         ((((bgr_row_8in10_scaled >> 10u) & 1023u) >> bgr_shift) << 8u) +
         ((bgr_row_8in10_scaled >> 20u) >> bgr_shift) +
         (((weights_alpha.xxxx >> weights_shifts_low) & 1u) * 0xFF000000u);
}

// Converts one row of four DXT3 alpha blocks to 16 packed R8 texels, useful for
// converting DXT3A. Only 16 bits of alpha half-blocks are used. Alpha is from
// word 0 for rows 0 and 1, from word 1 for rows 2 and 3, must be shifted right
// by 16 * (row index & 1) before calling.
uint4 XeDXT3FourBlocksRowToA8(uint4 alphas) {
  // (alphas & 0xFu) | ((alphas & 0xFu) << 4u) |
  // ((alphas & 0xF0u) << (8u - 4u)) | ((alphas & 0xF0u) << (12u - 4u)) |
  // ((alphas & 0xF00u) << (16u - 8u)) | ((alphas & 0xF00u) << (20u - 8u)) |
  // ((alphas & 0xF000u) << (24u - 12u)) | ((alphas & 0xF000u) << (28u - 12u))
  return (alphas & 0xFu) | ((alphas & 0xFFu) << 4u) |
         ((alphas & 0xFF0u) << 8u) | ((alphas & 0xFF00u) << 12u) |
         ((alphas & 0xF000u) << 16u);
}

uint4 XeDXT3AAs1111TwoBlocksRowToBGRA4(uint2 halfblocks) {
  // Only 16 bits of half-blocks are used. X contains pixels 0123, Y - 4567 (in
  // the image, halfblocks.y is halfblocks.x + 8).
  // In the row, X contains pixels 01, Y - 23, Z - 45, W - 67.
  // Assuming alpha in LSB and red in MSB, because it's consistent with how
  // DXT1/DXT3/DXT5 color components and CTX1 X/Y are ordered in:
  // http://fileadmin.cs.lth.se/cs/Personal/Michael_Doggett/talks/unc-xenos-doggett.pdf
  // (LSB on the right, MSB on the left.)
  // TODO(Triang3l): Investigate this better, 4D53085B is the only known game
  // that uses it (for lighting in certain places - one of easy to notice usages
  // is the T-shaped (or somewhat H-shaped) metal beams in the beginning of the
  // first mission), however the contents don't say anything about the channel
  // order.
  uint4 row = (((halfblocks.xxyy >> uint2(3u, 11u).xyxy) & 1u) << 8u) |
                (((halfblocks.xxyy >> uint2(7u, 15u).xyxy) & 1u) << 24u) |
                (((halfblocks.xxyy >> uint2(2u, 10u).xyxy) & 1u) << 4u) |
                (((halfblocks.xxyy >> uint2(6u, 14u).xyxy) & 1u) << 20u) |
                ((halfblocks.xxyy >> uint2(1u, 9u).xyxy) & 1u) |
                (((halfblocks.xxyy >> uint2(5u, 13u).xyxy) & 1u) << 16u) |
                (((halfblocks.xxyy >> uint2(0u, 8u).xyxy) & 1u) << 12u) |
                (((halfblocks.xxyy >> uint2(4u, 12u).xyxy) & 1u) << 28u);
  row |= row << 1u;
  row |= row << 2u;
  return row;
}

// & 0x249249 = bits 0 of 24 bits of DXT5 alpha codes.
// & 0x492492 = bits 1 of 24 bits of DXT5 alpha codes.
// & 0x924924 = bits 2 of 24 bits of DXT5 alpha codes.

// Sorts half (24 bits) of the codes of a DXT5 alpha block so they can be used
// as weights for the second endpoint, from 0 to 7, in alpha0 > alpha1 mode.
uint XeDXT5High8StepAlphaWeights(uint codes_24b) {
  // Initially 000 - first endpoint, 001 - second endpoint, 010 and above -
  // weights from 6:1 to 1:6. Need to make 001 111, and subtract 1 from 010 and
  // above.
  // Whether the bits are 000 (the first endpoint only).
  uint is_first = ((codes_24b & 0x249249u) | ((codes_24b & 0x492492u) >> 1u) |
                   ((codes_24b & 0x924924u) >> 2u)) ^ 0x249249u;
  // Whether the bits are 001 (the second endpoint only).
  uint is_second = (codes_24b & 0x249249u) & ~((codes_24b & 0x492492u) >> 1u) &
                   ~((codes_24b & 0x924924u) >> 2u);
  // Change 000 to 001 so subtracting 1 will result in 0 (and there will never
  // be overflow), subtract 1, and if the code was originally 001 (the second
  // endpoint only), make it 111.
  return ((codes_24b | is_first) - 0x249249u) |
         is_second | (is_second << 1u) | (is_second << 2u);
}

// Sorts half (24 bits) of the codes of a DXT5 alpha block so they can be used
// as weights for the second endpoint, from 0 to 5, in alpha0 <= alpha1 mode,
// except for 110 and 111 which represent 0 and 1 constants.
uint XeDXT5High6StepAlphaWeights(uint codes_24b) {
  // Initially:
  // 000 - first endpoint.
  // 001 - second endpoint.
  // 010 - 4:1.
  // 011 - 3:2.
  // 100 - 2:3.
  // 101 - 1:4.
  // 110 - constant 0.
  // 111 - constant 1.
  // Create 3-bit masks (111 or 000) of whether the codes represent 0 or 1
  // constants to keep them 110 and 111 later.
  uint is_constant = codes_24b & 0x492492u & ((codes_24b & 0x924924u) >> 1u);
  is_constant |= (is_constant << 1u) | (is_constant >> 1u);
  // Store the codes for the constants (110 or 111), or 0 if not a constant.
  uint constant_values =
      ((codes_24b & 0x249249u) | (0x492492u | 0x924924u)) & is_constant;
  // Need to make 001 101, and subtract 1 from 010 and above (constants will be
  // handled separately later).
  // Whether the bits are 000 (the first endpoint only).
  uint is_first = ((codes_24b & 0x249249u) | ((codes_24b & 0x492492u) >> 1u) |
                   ((codes_24b & 0x924924u) >> 2u)) ^ 0x249249u;
  // Whether the bits are 001 (the second endpoint only).
  uint is_second = (codes_24b & 0x249249u) & ~((codes_24b & 0x492492u) >> 1u) &
                   ~((codes_24b & 0x924924u) >> 2u);
  // Change 000 to 001 so subtracting 1 will result in 0 (and there will never
  // be overflow), subtract 1, and if the code was originally 001 (the second
  // endpoint only), make it 101.
  codes_24b =
      ((codes_24b | is_first) - 0x249249u) | is_second | (is_second << 2u);
  // Make constants 110 and 111 again (they are 101 and 110 now).
  return (codes_24b & ~is_constant) | constant_values;
}

// Sorts half (24 bits) of the codes of a DXT5 alpha block so they can be used
// as weights for XeDXT5RowToA8.
uint XeDXT5HighAlphaWeights(uint2 end, uint codes_24b) {
  return (end.x <= end.y) ? XeDXT5High6StepAlphaWeights(codes_24b)
                          : XeDXT5High8StepAlphaWeights(codes_24b);
}

// Get alphas of a DXT5 alpha row in alpha0 > alpha1 mode. Endpoint alphas are
// in bits 0:7 and 8:15 of the first dword, weights can be obtained using
// XeDXT5High8StepAlphaWeights and must be shifted right by 12 * (row index & 1)
// before calling.
uint XeDXT58StepRowToA8(uint2 end, uint weights_high) {
  uint weights_low = ~weights_high;
  return
      ((end.x * (weights_low & 7u) + end.y * (weights_high & 7u)) / 7u) |
      (((end.x * ((weights_low >> 3u) & 7u) +
         end.y * ((weights_high >> 3u) & 7u)) / 7u) << 8u) |
      (((end.x * ((weights_low >> 6u) & 7u) +
         end.y * ((weights_high >> 6u) & 7u)) / 7u) << 16u) |
      (((end.x * ((weights_low >> 9u) & 7u) +
         end.y * ((weights_high >> 9u) & 7u)) / 7u) << 24u);
}

// Version of XeDXT58StepRowToA8 that returns values packed in low 8 bits of
// 16-bit parts, for DXN decompression.
uint2 XeDXT58StepRowToA8In16(uint2 end, uint weights_high) {
  uint weights_low = ~weights_high;
  return uint2(
      ((end.x * (weights_low & 7u) + end.y * (weights_high & 7u)) / 7u) |
      (((end.x * ((weights_low >> 3u) & 7u) +
         end.y * ((weights_high >> 3u) & 7u)) / 7u) << 16u),
      ((end.x * ((weights_low >> 6u) & 7u) +
        end.y * ((weights_high >> 6u) & 7u)) / 7u) |
      (((end.x * ((weights_low >> 9u) & 7u) +
         end.y * ((weights_high >> 9u) & 7u)) / 7u) << 16u));
}

// Get alphas of a DXT5 alpha row in alpha0 <= alpha1 mode. Endpoint alphas are
// in bits 0:7 and 8:15 of the first dword, weights can be obtained using
// XeDXT5High6StepAlphaWeights and must be shifted right by 12 * (row index & 1)
// before calling.
uint XeDXT56StepRowToA8(uint2 end, uint weights_6step) {
  // Make a mask for whether the weights are constants.
  uint is_constant = weights_6step & 0x492u & ((weights_6step & 0x924u) >> 1u);
  is_constant |= (is_constant << 1u) | (is_constant >> 1u);
  // Get the weights for the first endpoint and remove constant from the
  // interpolation (set weights of the endpoints to 0 for them). First need to
  // zero the weights of the second endpoint so 6 or 7 won't be subtracted from
  // 5 while getting the weights of the first endpoint.
  uint weights_high = weights_6step & ~is_constant;
  uint weights_low = ((5u * 0x249u) - weights_high) & ~is_constant;
  // Interpolate.
  uint row =
      ((end.x * (weights_low & 7u) + end.y * (weights_high & 7u)) / 5u) |
      (((end.x * ((weights_low >> 3u) & 7u) +
         end.y * ((weights_high >> 3u) & 7u)) / 5u) << 8u) |
      (((end.x * ((weights_low >> 6u) & 7u) +
         end.y * ((weights_high >> 6u) & 7u)) / 5u) << 16u) |
      (((end.x * ((weights_low >> 9u) & 7u) +
         end.y * ((weights_high >> 9u) & 7u)) / 5u) << 24u);
  // Get the constant values as 1 bit per pixel separated by 7 bits.
  uint constant_values = weights_6step & is_constant;
  constant_values = (constant_values & 1u) |
                    ((constant_values & (1u << 3u)) << (8u - 3u)) |
                    ((constant_values & (1u << 6u)) << (16u - 6u)) |
                    ((constant_values & (1u << 9u)) << (24u - 9u));
  // Add constant 1 where needed.
  return row + constant_values * 0xFFu;
}

// Version of XeDXT56StepRowToA8 that returns values packed in low 8 bits of
// 16-bit parts, for DXN decompression.
uint2 XeDXT56StepRowToA8In16(uint2 end, uint weights_6step) {
  // Make a mask for whether the weights are constants.
  uint is_constant = weights_6step & 0x492u & ((weights_6step & 0x924u) >> 1u);
  is_constant |= (is_constant << 1u) | (is_constant >> 1u);
  // Get the weights for the first endpoint and remove constant from the
  // interpolation (set weights of the endpoints to 0 for them). First need to
  // zero the weights of the second endpoint so 6 or 7 won't be subtracted from
  // 5 while getting the weights of the first endpoint.
  uint weights_high = weights_6step & ~is_constant;
  uint weights_low = ((5u * 0x249u) - weights_high) & ~is_constant;
  // Interpolate.
  uint2 row = uint2(
      ((end.x * (weights_low & 7u) + end.y * (weights_high & 7u)) / 5u) |
      (((end.x * ((weights_low >> 3u) & 7u) +
         end.y * ((weights_high >> 3u) & 7u)) / 5u) << 16u),
      ((end.x * ((weights_low >> 6u) & 7u) +
        end.y * ((weights_high >> 6u) & 7u)) / 5u) |
      (((end.x * ((weights_low >> 9u) & 7u) +
         end.y * ((weights_high >> 9u) & 7u)) / 5u) << 16u));
  // Get the constant values as 1 bit per pixel separated by 7 bits.
  uint constant_weights = weights_6step & is_constant;
  uint2 constant_values = uint2(
      (constant_weights & 1u) | ((constant_weights & (1u << 3u)) << (16u - 3u)),
      ((constant_weights >> 6u) & 1u) |
          ((constant_weights & (1u << 9u)) << (16u - 9u)));
  // Add constant 1 where needed.
  return row + constant_values * 0xFFu;
}

// Get alphas of a DXT5 alpha row. Endpoint alphas are in bits 0:7 and 8:15 of
// the first dword, weights can be obtained using XeDXT5HighAlphaWeights and
// must be shifted right by 12 * (row index & 1) before calling.
uint XeDXT5RowToA8(uint2 end, uint weights) {
  return (end.x <= end.y) ? XeDXT56StepRowToA8(end, weights)
                          : XeDXT58StepRowToA8(end, weights);
}

// Version of XeDXT5RowToA8 that returns values packed in low 8 bits of 16-bit
// parts, for DXN decompression.
uint2 XeDXT5RowToA8In16(uint2 end, uint weights) {
  return (end.x <= end.y) ? XeDXT56StepRowToA8In16(end, weights)
                          : XeDXT58StepRowToA8In16(end, weights);
}

// Converts one row of two CTX1 blocks to R8G8. Endpoints of block 0 in XY and
// of block 1 in ZW must be unpacked from 0xRRGGrrgg to 0x00gg00rr 0x00GG00RR so
// they can be multiplied by weights with room for overflow. Weights can be
// obtained using XeDXTHighColorWeights and must be shifted right by 8 * row
// index before calling.
uint4 XeCTX1TwoBlocksRowToR8G8(uint4 end_8in16, uint2 weights_high) {
  uint2 weights_low = ~weights_high;
  const uint4 weights_shifts = uint4(0u, 2u, 4u, 6u);
  uint4 row_8in16 = ((weights_low.xxxx >> weights_shifts) & 3u) * end_8in16.x +
                    ((weights_high.xxxx >> weights_shifts) & 3u) * end_8in16.y;
  uint4 result;
  result.xy = ((row_8in16.xz & 0xFFFFu) / 3u) |
              (((row_8in16.xz >> 16u) / 3u) << 8u) |
              (((row_8in16.yw & 0xFFFFu) / 3u) << 16u) |
              (((row_8in16.yw >> 16u) / 3u) << 24u);
  row_8in16 = ((weights_low.yyyy >> weights_shifts) & 3u) * end_8in16.z +
              ((weights_high.yyyy >> weights_shifts) & 3u) * end_8in16.w;
  result.zw = ((row_8in16.xz & 0xFFFFu) / 3u) |
              (((row_8in16.xz >> 16u) / 3u) << 8u) |
              (((row_8in16.yw & 0xFFFFu) / 3u) << 16u) |
              (((row_8in16.yw >> 16u) / 3u) << 24u);
  return result;
}

#endif  // XENIA_GPU_D3D12_SHADERS_PIXEL_FORMATS_HLSLI_
