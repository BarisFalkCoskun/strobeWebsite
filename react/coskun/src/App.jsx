import { useState, useEffect, useRef, useMemo, Suspense, useCallback } from 'react'
import { Canvas, useFrame, useThree, extend } from '@react-three/fiber'
import { shaderMaterial, useTexture, Text, Float } from '@react-three/drei'
import { EffectComposer, Bloom, ChromaticAberration, Vignette, Noise, ToneMapping } from '@react-three/postprocessing'
import { BlendFunction, ToneMappingMode } from 'postprocessing'
import * as THREE from 'three'
import './App.css'

// Color themes
const themes = {
  cosmic: {
    primary: '#a855f7',
    secondary: '#06b6d4',
    accent: '#f59e0b',
    colors: ['#ff006e', '#fb5607', '#ffbe0b', '#8338ec', '#3a86ff', '#06d6a0'],
  },
  neon: {
    primary: '#ff00ff',
    secondary: '#00ffff',
    accent: '#ffff00',
    colors: ['#ff00ff', '#ff0080', '#ff0000', '#ff8000', '#00ff00', '#00ffff'],
  },
  ocean: {
    primary: '#0077b6',
    secondary: '#00b4d8',
    accent: '#90e0ef',
    colors: ['#03045e', '#023e8a', '#0077b6', '#0096c7', '#00b4d8', '#48cae4'],
  },
  sunset: {
    primary: '#ff6b6b',
    secondary: '#feca57',
    accent: '#ff9ff3',
    colors: ['#ff6b6b', '#ee5a24', '#f79f1f', '#feca57', '#ff9ff3', '#c44569'],
  },
  matrix: {
    primary: '#00ff00',
    secondary: '#003300',
    accent: '#00ff00',
    colors: ['#00ff00', '#00cc00', '#009900', '#006600', '#00ff00', '#33ff33'],
  },
}

// Fractal noise shader for nebula effect
const NebulaMaterial = shaderMaterial(
  {
    uTime: 0,
    uStrobe: 0,
    uMouse: new THREE.Vector2(0.5, 0.5),
    uResolution: new THREE.Vector2(1, 1),
    uBass: 0,
    uTheme: 0,
    uZenMode: 0,
  },
  `
    varying vec2 vUv;
    void main() {
      vUv = uv;
      gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
    }
  `,
  `
    uniform float uTime;
    uniform float uStrobe;
    uniform vec2 uMouse;
    uniform float uBass;
    uniform float uTheme;
    uniform float uZenMode;
    varying vec2 vUv;

    #define PI 3.14159265359

    // Simplex noise functions
    vec3 mod289(vec3 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
    vec4 mod289(vec4 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
    vec4 permute(vec4 x) { return mod289(((x*34.0)+1.0)*x); }
    vec4 taylorInvSqrt(vec4 r) { return 1.79284291400159 - 0.85373472095314 * r; }

    float snoise(vec3 v) {
      const vec2 C = vec2(1.0/6.0, 1.0/3.0);
      const vec4 D = vec4(0.0, 0.5, 1.0, 2.0);

      vec3 i  = floor(v + dot(v, C.yyy));
      vec3 x0 = v - i + dot(i, C.xxx);

      vec3 g = step(x0.yzx, x0.xyz);
      vec3 l = 1.0 - g;
      vec3 i1 = min(g.xyz, l.zxy);
      vec3 i2 = max(g.xyz, l.zxy);

      vec3 x1 = x0 - i1 + C.xxx;
      vec3 x2 = x0 - i2 + C.yyy;
      vec3 x3 = x0 - D.yyy;

      i = mod289(i);
      vec4 p = permute(permute(permute(
                i.z + vec4(0.0, i1.z, i2.z, 1.0))
              + i.y + vec4(0.0, i1.y, i2.y, 1.0))
              + i.x + vec4(0.0, i1.x, i2.x, 1.0));

      float n_ = 0.142857142857;
      vec3 ns = n_ * D.wyz - D.xzx;

      vec4 j = p - 49.0 * floor(p * ns.z * ns.z);

      vec4 x_ = floor(j * ns.z);
      vec4 y_ = floor(j - 7.0 * x_);

      vec4 x = x_ *ns.x + ns.yyyy;
      vec4 y = y_ *ns.x + ns.yyyy;
      vec4 h = 1.0 - abs(x) - abs(y);

      vec4 b0 = vec4(x.xy, y.xy);
      vec4 b1 = vec4(x.zw, y.zw);

      vec4 s0 = floor(b0)*2.0 + 1.0;
      vec4 s1 = floor(b1)*2.0 + 1.0;
      vec4 sh = -step(h, vec4(0.0));

      vec4 a0 = b0.xzyw + s0.xzyw*sh.xxyy;
      vec4 a1 = b1.xzyw + s1.xzyw*sh.zzww;

      vec3 p0 = vec3(a0.xy, h.x);
      vec3 p1 = vec3(a0.zw, h.y);
      vec3 p2 = vec3(a1.xy, h.z);
      vec3 p3 = vec3(a1.zw, h.w);

      vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2,p2), dot(p3,p3)));
      p0 *= norm.x;
      p1 *= norm.y;
      p2 *= norm.z;
      p3 *= norm.w;

      vec4 m = max(0.6 - vec4(dot(x0,x0), dot(x1,x1), dot(x2,x2), dot(x3,x3)), 0.0);
      m = m * m;
      return 42.0 * dot(m*m, vec4(dot(p0,x0), dot(p1,x1), dot(p2,x2), dot(p3,x3)));
    }

    float fbm(vec3 p) {
      float value = 0.0;
      float amplitude = 0.5;
      float frequency = 1.0;
      for (int i = 0; i < 6; i++) {
        value += amplitude * snoise(p * frequency);
        amplitude *= 0.5;
        frequency *= 2.0;
      }
      return value;
    }

    vec3 hsv2rgb(vec3 c) {
      vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
      vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
      return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
    }

    void main() {
      vec2 uv = vUv;
      vec2 center = uv - 0.5;
      float dist = length(center);

      // Slower time in zen mode
      float timeSpeed = uZenMode > 0.5 ? 0.05 : 0.15;
      float t = uTime * timeSpeed;

      // Multi-layer nebula
      float n1 = fbm(vec3(uv * 3.0, t));
      float n2 = fbm(vec3(uv * 5.0 + 100.0, t * 1.3));
      float n3 = fbm(vec3(uv * 8.0 + 200.0, t * 0.7));

      float nebula = n1 * 0.5 + n2 * 0.3 + n3 * 0.2;
      nebula = nebula * 0.5 + 0.5;

      // Theme-based color palette
      float hueOffset = uTheme * 0.2;
      float hue1 = 0.75 + sin(t * 0.5) * 0.1 + hueOffset;
      float hue2 = 0.55 + cos(t * 0.3) * 0.1 + hueOffset;
      float hue3 = 0.85 + hueOffset;

      // Lower saturation in zen mode
      float sat = uZenMode > 0.5 ? 0.4 : 0.8;

      vec3 col1 = hsv2rgb(vec3(hue1, sat, 0.6));
      vec3 col2 = hsv2rgb(vec3(hue2, sat * 0.9, 0.5));
      vec3 col3 = hsv2rgb(vec3(hue3, sat * 1.1, 0.7));

      vec3 color = mix(col1, col2, nebula);
      color = mix(color, col3, n3 * 0.5 + 0.25);

      // Radial fade
      float vignette = 1.0 - smoothstep(0.2, 0.8, dist);
      color *= vignette * 0.8;

      // Add glow spots
      float spots = pow(max(0.0, n1), 3.0);
      color += spots * col3 * 0.5;

      // Strobe effect
      if (uStrobe > 0.5) {
        float flash = sin(uTime * 30.0) * 0.5 + 0.5;
        float strobeHue = fract(uTime * 2.0);
        vec3 strobeColor = hsv2rgb(vec3(strobeHue, 1.0, 1.0));
        color = mix(color, strobeColor, flash * 0.5);
        color *= 1.0 + flash;
      }

      // Bass reactive pulse
      color *= 1.0 + uBass * 0.3;

      gl_FragColor = vec4(color, 1.0);
    }
  `
)

extend({ NebulaMaterial })

// Portal/Wormhole shader
const PortalMaterial = shaderMaterial(
  { uTime: 0, uStrobe: 0 },
  `
    varying vec2 vUv;
    varying vec3 vPosition;
    void main() {
      vUv = uv;
      vPosition = position;
      gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
    }
  `,
  `
    uniform float uTime;
    uniform float uStrobe;
    varying vec2 vUv;

    #define PI 3.14159265359

    void main() {
      vec2 center = vUv - 0.5;
      float dist = length(center);
      float angle = atan(center.y, center.x);

      // Swirling effect
      float swirl = sin(angle * 8.0 - uTime * 3.0 + dist * 20.0) * 0.5 + 0.5;
      swirl *= sin(angle * 5.0 + uTime * 2.0 - dist * 15.0) * 0.5 + 0.5;

      // Tunnel depth
      float tunnel = 1.0 / (dist * 10.0 + 0.5);
      tunnel = pow(tunnel, 0.5);

      // Color
      vec3 color1 = vec3(0.5, 0.0, 1.0);
      vec3 color2 = vec3(0.0, 1.0, 1.0);
      vec3 color = mix(color1, color2, swirl);

      // Edge glow
      float edge = smoothstep(0.5, 0.3, dist);
      color *= edge * tunnel;

      // Strobe boost
      if (uStrobe > 0.5) {
        color *= 2.0;
        color += vec3(sin(uTime * 20.0) * 0.3);
      }

      float alpha = edge * (0.6 + swirl * 0.4);
      gl_FragColor = vec4(color, alpha * 0.7);
    }
  `
)

extend({ PortalMaterial })

// Volumetric light rays shader
const GodRaysMaterial = shaderMaterial(
  { uTime: 0, uStrobe: 0 },
  `
    varying vec2 vUv;
    varying vec3 vPosition;
    void main() {
      vUv = uv;
      vPosition = position;
      gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
    }
  `,
  `
    uniform float uTime;
    uniform float uStrobe;
    varying vec2 vUv;
    varying vec3 vPosition;

    void main() {
      vec2 center = vUv - 0.5;
      float angle = atan(center.y, center.x);
      float dist = length(center);

      // Rotating rays
      float rays = sin(angle * 12.0 + uTime * 2.0) * 0.5 + 0.5;
      rays = pow(rays, 3.0);

      // Radial falloff
      float falloff = 1.0 - smoothstep(0.0, 0.5, dist);
      falloff = pow(falloff, 2.0);

      float intensity = rays * falloff;

      // Color
      vec3 color = mix(
        vec3(0.6, 0.2, 0.8),
        vec3(0.2, 0.8, 0.9),
        sin(angle * 3.0 + uTime) * 0.5 + 0.5
      );

      if (uStrobe > 0.5) {
        float flash = sin(uTime * 25.0) * 0.5 + 0.5;
        color = mix(color, vec3(1.0), flash * 0.5);
        intensity *= 1.5;
      }

      gl_FragColor = vec4(color * intensity, intensity * 0.6);
    }
  `
)

extend({ GodRaysMaterial })

// Holographic material
const HologramMaterial = shaderMaterial(
  { uTime: 0, uStrobe: 0, uColor: new THREE.Color(0.5, 0.0, 1.0) },
  `
    varying vec2 vUv;
    varying vec3 vNormal;
    varying vec3 vPosition;
    varying vec3 vWorldPosition;

    void main() {
      vUv = uv;
      vNormal = normalize(normalMatrix * normal);
      vPosition = position;
      vWorldPosition = (modelMatrix * vec4(position, 1.0)).xyz;
      gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
    }
  `,
  `
    uniform float uTime;
    uniform float uStrobe;
    uniform vec3 uColor;
    varying vec2 vUv;
    varying vec3 vNormal;
    varying vec3 vPosition;
    varying vec3 vWorldPosition;

    void main() {
      // Scanlines
      float scanline = sin(vWorldPosition.y * 100.0 + uTime * 10.0) * 0.5 + 0.5;
      scanline = pow(scanline, 0.5);

      // Fresnel
      vec3 viewDirection = normalize(cameraPosition - vWorldPosition);
      float fresnel = pow(1.0 - abs(dot(viewDirection, vNormal)), 3.0);

      // Glitch offset
      float glitch = step(0.98, sin(uTime * 50.0 + vWorldPosition.y * 20.0));

      // Color
      vec3 color = uColor;
      color += fresnel * vec3(0.3, 0.6, 1.0);
      color *= scanline * 0.5 + 0.5;

      // Hologram flicker
      float flicker = sin(uTime * 30.0) * 0.1 + 0.9;
      color *= flicker;

      if (uStrobe > 0.5) {
        float flash = sin(uTime * 40.0) * 0.5 + 0.5;
        color = mix(color, vec3(1.0), flash * 0.3);
      }

      float alpha = fresnel * 0.8 + 0.2;
      alpha *= scanline * 0.3 + 0.7;

      gl_FragColor = vec4(color, alpha);
    }
  `
)

extend({ HologramMaterial })

// Aurora shader
const AuroraMaterial = shaderMaterial(
  { uTime: 0, uStrobe: 0 },
  `
    varying vec2 vUv;
    void main() {
      vUv = uv;
      gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
    }
  `,
  `
    uniform float uTime;
    uniform float uStrobe;
    varying vec2 vUv;

    float noise(vec2 p) {
      return sin(p.x * 10.0) * sin(p.y * 10.0);
    }

    void main() {
      vec2 uv = vUv;

      // Wavy aurora bands
      float wave1 = sin(uv.x * 5.0 + uTime + sin(uv.y * 3.0) * 2.0) * 0.5 + 0.5;
      float wave2 = sin(uv.x * 8.0 - uTime * 0.7 + cos(uv.y * 4.0) * 3.0) * 0.5 + 0.5;
      float wave3 = sin(uv.x * 3.0 + uTime * 1.3 + sin(uv.y * 2.0) * 1.5) * 0.5 + 0.5;

      // Combine waves
      float aurora = wave1 * wave2 * wave3;
      aurora = pow(aurora, 2.0);

      // Vertical fade
      float fade = smoothstep(0.0, 0.3, uv.y) * smoothstep(1.0, 0.7, uv.y);

      // Colors
      vec3 green = vec3(0.2, 1.0, 0.4);
      vec3 purple = vec3(0.6, 0.2, 1.0);
      vec3 blue = vec3(0.2, 0.5, 1.0);

      vec3 color = mix(green, purple, wave1);
      color = mix(color, blue, wave2 * 0.5);

      float intensity = aurora * fade;

      if (uStrobe > 0.5) {
        intensity *= 2.0;
        color = mix(color, vec3(1.0), sin(uTime * 30.0) * 0.3);
      }

      gl_FragColor = vec4(color * intensity, intensity * 0.5);
    }
  `
)

extend({ AuroraMaterial })

// Nebula background
function NebulaBackground({ strobeActive, audioData, themeIndex, zenMode }) {
  const material = useRef()
  const { viewport } = useThree()

  useFrame((state) => {
    if (material.current) {
      material.current.uTime = state.clock.elapsedTime
      material.current.uStrobe = strobeActive ? 1.0 : 0.0
      material.current.uBass = audioData?.bass || 0
      material.current.uTheme = themeIndex
      material.current.uZenMode = zenMode ? 1.0 : 0.0
    }
  })

  return (
    <mesh position={[0, 0, -20]} scale={[viewport.width * 2, viewport.height * 2, 1]}>
      <planeGeometry args={[1, 1, 1, 1]} />
      <nebulaMaterial ref={material} />
    </mesh>
  )
}

// Portal effect
function Portal({ strobeActive }) {
  const material = useRef()
  const mesh = useRef()

  useFrame((state) => {
    if (material.current) {
      material.current.uTime = state.clock.elapsedTime
      material.current.uStrobe = strobeActive ? 1.0 : 0.0
    }
    if (mesh.current) {
      mesh.current.rotation.z = state.clock.elapsedTime * 0.1
    }
  })

  return (
    <mesh ref={mesh} position={[0, 0, -8]}>
      <circleGeometry args={[4, 64]} />
      <portalMaterial ref={material} transparent side={THREE.DoubleSide} blending={THREE.AdditiveBlending} />
    </mesh>
  )
}

// Aurora effect
function Aurora({ strobeActive, zenMode }) {
  const material = useRef()

  useFrame((state) => {
    if (material.current) {
      material.current.uTime = state.clock.elapsedTime * (zenMode ? 0.3 : 1)
      material.current.uStrobe = strobeActive ? 1.0 : 0.0
    }
  })

  return (
    <mesh position={[0, 5, -15]} rotation={[-Math.PI / 6, 0, 0]}>
      <planeGeometry args={[30, 10]} />
      <auroraMaterial ref={material} transparent side={THREE.DoubleSide} blending={THREE.AdditiveBlending} />
    </mesh>
  )
}

// God rays
function GodRays({ strobeActive }) {
  const material = useRef()

  useFrame((state) => {
    if (material.current) {
      material.current.uTime = state.clock.elapsedTime
      material.current.uStrobe = strobeActive ? 1.0 : 0.0
    }
  })

  return (
    <mesh position={[0, 0, -5]}>
      <planeGeometry args={[20, 20]} />
      <godRaysMaterial ref={material} transparent blending={THREE.AdditiveBlending} />
    </mesh>
  )
}

// Central holographic shape with morphing
function HolographicCore({ strobeActive, zenMode }) {
  const mesh = useRef()
  const material = useRef()
  const [morphTarget, setMorphTarget] = useState(0)

  useEffect(() => {
    if (!zenMode) {
      const interval = setInterval(() => {
        setMorphTarget(prev => (prev + 1) % 3)
      }, 5000)
      return () => clearInterval(interval)
    }
  }, [zenMode])

  useFrame((state) => {
    if (mesh.current) {
      const speed = zenMode ? 0.05 : 0.2
      mesh.current.rotation.x = state.clock.elapsedTime * speed
      mesh.current.rotation.y = state.clock.elapsedTime * speed * 1.5

      const scale = 1 + Math.sin(state.clock.elapsedTime * 2) * 0.1
      mesh.current.scale.setScalar(strobeActive ? scale * 1.3 : scale)
    }
    if (material.current) {
      material.current.uTime = state.clock.elapsedTime
      material.current.uStrobe = strobeActive ? 1.0 : 0.0
    }
  })

  return (
    <mesh ref={mesh}>
      {morphTarget === 0 && <octahedronGeometry args={[1.5, 0]} />}
      {morphTarget === 1 && <icosahedronGeometry args={[1.5, 0]} />}
      {morphTarget === 2 && <dodecahedronGeometry args={[1.5, 0]} />}
      <hologramMaterial
        ref={material}
        transparent
        side={THREE.DoubleSide}
        depthWrite={false}
        blending={THREE.AdditiveBlending}
      />
    </mesh>
  )
}

// DNA Helix
function DNAHelix({ strobeActive }) {
  const group = useRef()
  const helixPoints = useMemo(() => {
    const points = []
    for (let i = 0; i < 100; i++) {
      const t = i / 100 * Math.PI * 4
      const y = (i / 100 - 0.5) * 10
      points.push({
        pos1: new THREE.Vector3(Math.cos(t) * 1.5, y, Math.sin(t) * 1.5),
        pos2: new THREE.Vector3(Math.cos(t + Math.PI) * 1.5, y, Math.sin(t + Math.PI) * 1.5),
        t,
      })
    }
    return points
  }, [])

  useFrame((state) => {
    if (group.current) {
      group.current.rotation.y = state.clock.elapsedTime * 0.3
      group.current.children.forEach((child, i) => {
        if (child.material) {
          const pulse = Math.sin(state.clock.elapsedTime * 3 + i * 0.1) * 0.5 + 0.5
          child.material.opacity = strobeActive ? 0.9 : 0.4 + pulse * 0.3
        }
      })
    }
  })

  return (
    <group ref={group} position={[6, 0, -5]}>
      {helixPoints.map((point, i) => (
        <group key={i}>
          <mesh position={point.pos1}>
            <sphereGeometry args={[0.08, 8, 8]} />
            <meshBasicMaterial color={strobeActive ? '#ffffff' : '#ff006e'} transparent opacity={0.6} />
          </mesh>
          <mesh position={point.pos2}>
            <sphereGeometry args={[0.08, 8, 8]} />
            <meshBasicMaterial color={strobeActive ? '#ffffff' : '#00ffff'} transparent opacity={0.6} />
          </mesh>
          {i % 5 === 0 && (
            <line>
              <bufferGeometry>
                <bufferAttribute
                  attach="attributes-position"
                  count={2}
                  array={new Float32Array([...point.pos1.toArray(), ...point.pos2.toArray()])}
                  itemSize={3}
                />
              </bufferGeometry>
              <lineBasicMaterial color="#8338ec" transparent opacity={0.3} />
            </line>
          )}
        </group>
      ))}
    </group>
  )
}

// Lightning bolt effect
function Lightning({ strobeActive }) {
  const lines = useRef()
  const [bolts, setBolts] = useState([])

  const generateBolt = useCallback(() => {
    const points = []
    let x = 0, y = 5, z = 0
    const segments = 15

    for (let i = 0; i < segments; i++) {
      points.push(new THREE.Vector3(x, y, z))
      x += (Math.random() - 0.5) * 2
      y -= 10 / segments + Math.random() * 0.5
      z += (Math.random() - 0.5) * 1
    }
    return points
  }, [])

  useEffect(() => {
    if (!strobeActive) {
      setBolts([])
      return
    }

    const interval = setInterval(() => {
      setBolts([
        { points: generateBolt(), key: Date.now(), offset: -8 },
        { points: generateBolt(), key: Date.now() + 1, offset: 8 },
      ])
      setTimeout(() => setBolts([]), 100)
    }, 200)

    return () => clearInterval(interval)
  }, [strobeActive, generateBolt])

  return (
    <group>
      {bolts.map(bolt => (
        <line key={bolt.key}>
          <bufferGeometry>
            <bufferAttribute
              attach="attributes-position"
              count={bolt.points.length}
              array={new Float32Array(bolt.points.flatMap(p => [p.x + bolt.offset, p.y, p.z - 5]))}
              itemSize={3}
            />
          </bufferGeometry>
          <lineBasicMaterial color="#ffffff" linewidth={3} transparent opacity={0.9} />
        </line>
      ))}
    </group>
  )
}

// Meteor shower
function MeteorShower({ strobeActive }) {
  const meteors = useRef([])
  const count = 20

  const meteorData = useMemo(() => {
    return Array.from({ length: count }, () => ({
      position: new THREE.Vector3(
        (Math.random() - 0.5) * 30,
        Math.random() * 10 + 5,
        (Math.random() - 0.5) * 20 - 10
      ),
      velocity: new THREE.Vector3(-0.1 - Math.random() * 0.2, -0.2 - Math.random() * 0.1, 0),
      size: Math.random() * 0.1 + 0.02,
    }))
  }, [])

  useFrame(() => {
    meteors.current.forEach((meteor, i) => {
      if (meteor) {
        const data = meteorData[i]
        data.position.add(data.velocity)

        // Reset when out of view
        if (data.position.y < -10 || data.position.x < -20) {
          data.position.set(
            (Math.random() - 0.5) * 30 + 15,
            Math.random() * 10 + 5,
            (Math.random() - 0.5) * 20 - 10
          )
        }

        meteor.position.copy(data.position)
      }
    })
  })

  return (
    <group>
      {meteorData.map((data, i) => (
        <mesh key={i} ref={el => meteors.current[i] = el} position={data.position}>
          <sphereGeometry args={[data.size, 4, 4]} />
          <meshBasicMaterial
            color={strobeActive ? '#ffffff' : '#ffbe0b'}
            transparent
            opacity={0.8}
          />
        </mesh>
      ))}
    </group>
  )
}

// Click explosion particles
function ClickExplosion({ position, onComplete }) {
  const particles = useRef()
  const [particleData] = useState(() => {
    const count = 50
    const positions = new Float32Array(count * 3)
    const velocities = []

    for (let i = 0; i < count; i++) {
      positions[i * 3] = position.x
      positions[i * 3 + 1] = position.y
      positions[i * 3 + 2] = position.z

      velocities.push(new THREE.Vector3(
        (Math.random() - 0.5) * 0.5,
        (Math.random() - 0.5) * 0.5,
        (Math.random() - 0.5) * 0.5
      ))
    }

    return { positions, velocities, startTime: Date.now() }
  })

  useFrame(() => {
    if (!particles.current) return

    const elapsed = (Date.now() - particleData.startTime) / 1000
    const positions = particles.current.geometry.attributes.position.array

    for (let i = 0; i < particleData.velocities.length; i++) {
      const vel = particleData.velocities[i]
      positions[i * 3] += vel.x * 0.1
      positions[i * 3 + 1] += vel.y * 0.1 - elapsed * 0.02 // gravity
      positions[i * 3 + 2] += vel.z * 0.1
    }

    particles.current.geometry.attributes.position.needsUpdate = true
    particles.current.material.opacity = Math.max(0, 1 - elapsed)

    if (elapsed > 1) {
      onComplete()
    }
  })

  return (
    <points ref={particles}>
      <bufferGeometry>
        <bufferAttribute
          attach="attributes-position"
          count={50}
          array={particleData.positions}
          itemSize={3}
        />
      </bufferGeometry>
      <pointsMaterial
        size={0.1}
        color="#ffffff"
        transparent
        opacity={1}
        blending={THREE.AdditiveBlending}
      />
    </points>
  )
}

// Orbiting particles with trails
function OrbitingParticles({ strobeActive, zenMode }) {
  const particles = useRef()
  const count = zenMode ? 200 : 500
  const radius = 4

  const [positions, velocities] = useMemo(() => {
    const pos = new Float32Array(count * 3)
    const vel = new Float32Array(count * 3)

    for (let i = 0; i < count; i++) {
      const theta = Math.random() * Math.PI * 2
      const phi = Math.acos(2 * Math.random() - 1)
      const r = radius + (Math.random() - 0.5) * 2

      pos[i * 3] = r * Math.sin(phi) * Math.cos(theta)
      pos[i * 3 + 1] = r * Math.sin(phi) * Math.sin(theta)
      pos[i * 3 + 2] = r * Math.cos(phi)

      // Orbital velocity
      vel[i * 3] = (Math.random() - 0.5) * 0.02
      vel[i * 3 + 1] = (Math.random() - 0.5) * 0.02
      vel[i * 3 + 2] = (Math.random() - 0.5) * 0.02
    }

    return [pos, vel]
  }, [count])

  useFrame((state) => {
    if (particles.current) {
      const positions = particles.current.geometry.attributes.position.array
      const time = state.clock.elapsedTime

      for (let i = 0; i < count; i++) {
        const i3 = i * 3

        // Orbit around center
        const x = positions[i3]
        const y = positions[i3 + 1]
        const z = positions[i3 + 2]

        const speed = zenMode ? 0.3 : 1
        const angle = 0.01 * (strobeActive ? 3 : 1) * speed
        const cosA = Math.cos(angle)
        const sinA = Math.sin(angle)

        positions[i3] = x * cosA - z * sinA
        positions[i3 + 2] = x * sinA + z * cosA

        // Slight y oscillation
        positions[i3 + 1] += Math.sin(time * 2 + i * 0.1) * 0.002
      }

      particles.current.geometry.attributes.position.needsUpdate = true
      particles.current.rotation.y = time * 0.05
    }
  })

  return (
    <points ref={particles}>
      <bufferGeometry>
        <bufferAttribute attach="attributes-position" count={count} array={positions} itemSize={3} />
      </bufferGeometry>
      <pointsMaterial
        size={zenMode ? 0.05 : 0.03}
        color={strobeActive ? '#ffffff' : '#a855f7'}
        transparent
        opacity={zenMode ? 0.5 : 0.8}
        blending={THREE.AdditiveBlending}
        sizeAttenuation
      />
    </points>
  )
}

// Floating geometric shapes
function FloatingGeometries({ strobeActive, zenMode }) {
  const group = useRef()

  const geometries = useMemo(() => {
    const count = zenMode ? 8 : 15
    return Array.from({ length: count }, (_, i) => ({
      position: new THREE.Vector3(
        (Math.random() - 0.5) * 12,
        (Math.random() - 0.5) * 8,
        (Math.random() - 0.5) * 10 - 5
      ),
      rotation: new THREE.Euler(
        Math.random() * Math.PI,
        Math.random() * Math.PI,
        Math.random() * Math.PI
      ),
      scale: Math.random() * 0.3 + 0.1,
      speed: Math.random() * 0.5 + 0.2,
      type: ['tetrahedron', 'octahedron', 'icosahedron', 'torus', 'torusKnot'][Math.floor(Math.random() * 5)],
      color: new THREE.Color().setHSL(Math.random() * 0.3 + 0.7, 0.8, 0.5),
    }))
  }, [zenMode])

  useFrame((state) => {
    if (group.current) {
      group.current.children.forEach((mesh, i) => {
        const data = geometries[i]
        if (!data) return
        const speed = zenMode ? 0.2 : 1
        const t = state.clock.elapsedTime * data.speed * speed

        mesh.rotation.x = data.rotation.x + t
        mesh.rotation.y = data.rotation.y + t * 0.7
        mesh.position.y = data.position.y + Math.sin(t * 2) * 0.3

        if (strobeActive) {
          mesh.material.emissiveIntensity = 2 + Math.sin(t * 20)
        } else {
          mesh.material.emissiveIntensity = zenMode ? 0.2 : 0.5
        }
      })
    }
  })

  return (
    <group ref={group}>
      {geometries.map((geo, i) => (
        <mesh key={i} position={geo.position} scale={geo.scale}>
          {geo.type === 'tetrahedron' && <tetrahedronGeometry args={[1, 0]} />}
          {geo.type === 'octahedron' && <octahedronGeometry args={[1, 0]} />}
          {geo.type === 'icosahedron' && <icosahedronGeometry args={[1, 0]} />}
          {geo.type === 'torus' && <torusGeometry args={[1, 0.3, 8, 16]} />}
          {geo.type === 'torusKnot' && <torusKnotGeometry args={[0.8, 0.2, 64, 8]} />}
          <meshStandardMaterial
            color={geo.color}
            emissive={geo.color}
            emissiveIntensity={0.5}
            wireframe
            transparent
            opacity={zenMode ? 0.3 : 0.6}
          />
        </mesh>
      ))}
    </group>
  )
}

// Connecting lines between points
function ConnectionLines({ strobeActive }) {
  const lines = useRef()
  const pointCount = 30
  const maxDistance = 3

  const points = useMemo(() => {
    return Array.from({ length: pointCount }, () => ({
      position: new THREE.Vector3(
        (Math.random() - 0.5) * 10,
        (Math.random() - 0.5) * 6,
        (Math.random() - 0.5) * 6
      ),
      velocity: new THREE.Vector3(
        (Math.random() - 0.5) * 0.01,
        (Math.random() - 0.5) * 0.01,
        (Math.random() - 0.5) * 0.01
      ),
    }))
  }, [])

  useFrame((state) => {
    if (!lines.current) return

    // Update point positions
    points.forEach((point) => {
      point.position.add(point.velocity)

      // Bounce off boundaries
      if (Math.abs(point.position.x) > 5) point.velocity.x *= -1
      if (Math.abs(point.position.y) > 3) point.velocity.y *= -1
      if (Math.abs(point.position.z) > 3) point.velocity.z *= -1
    })

    // Build line geometry
    const linePositions = []
    for (let i = 0; i < points.length; i++) {
      for (let j = i + 1; j < points.length; j++) {
        const dist = points[i].position.distanceTo(points[j].position)
        if (dist < maxDistance) {
          linePositions.push(
            points[i].position.x, points[i].position.y, points[i].position.z,
            points[j].position.x, points[j].position.y, points[j].position.z
          )
        }
      }
    }

    const positionArray = new Float32Array(linePositions)
    lines.current.geometry.setAttribute('position', new THREE.BufferAttribute(positionArray, 3))
  })

  return (
    <lineSegments ref={lines}>
      <bufferGeometry />
      <lineBasicMaterial
        color={strobeActive ? '#ffffff' : '#06b6d4'}
        transparent
        opacity={0.3}
        blending={THREE.AdditiveBlending}
      />
    </lineSegments>
  )
}

// Energy waves
function EnergyWaves({ strobeActive }) {
  const waves = useRef([])

  useFrame((state) => {
    waves.current.forEach((wave, i) => {
      if (wave) {
        const scale = ((state.clock.elapsedTime * 0.5 + i * 0.3) % 3) * 3
        wave.scale.setScalar(scale)
        wave.material.opacity = Math.max(0, 1 - scale / 9) * (strobeActive ? 0.8 : 0.4)
      }
    })
  })

  return (
    <group>
      {[0, 1, 2].map((i) => (
        <mesh
          key={i}
          ref={(el) => (waves.current[i] = el)}
          rotation={[-Math.PI / 2, 0, 0]}
        >
          <ringGeometry args={[0.9, 1, 64]} />
          <meshBasicMaterial
            color={strobeActive ? '#ffffff' : '#a855f7'}
            transparent
            opacity={0.4}
            side={THREE.DoubleSide}
            blending={THREE.AdditiveBlending}
          />
        </mesh>
      ))}
    </group>
  )
}

// Light pillars
function LightPillars({ strobeActive }) {
  const pillars = useRef([])

  useFrame((state) => {
    pillars.current.forEach((pillar, i) => {
      if (pillar) {
        const t = state.clock.elapsedTime + i * 0.5
        pillar.material.opacity = (Math.sin(t * 2) * 0.3 + 0.5) * (strobeActive ? 1.5 : 1)
        pillar.scale.y = 1 + Math.sin(t * 3) * 0.2
      }
    })
  })

  const positions = useMemo(() => [
    [-6, 0, -8],
    [-3, 0, -10],
    [0, 0, -12],
    [3, 0, -10],
    [6, 0, -8],
  ], [])

  return (
    <group>
      {positions.map((pos, i) => (
        <mesh
          key={i}
          ref={(el) => (pillars.current[i] = el)}
          position={pos}
        >
          <cylinderGeometry args={[0.1, 0.3, 20, 8, 1, true]} />
          <meshBasicMaterial
            color={i % 2 === 0 ? '#a855f7' : '#06b6d4'}
            transparent
            opacity={0.5}
            side={THREE.DoubleSide}
            blending={THREE.AdditiveBlending}
          />
        </mesh>
      ))}
    </group>
  )
}

// Post-processing
function Effects({ strobeActive, zenMode }) {
  const bloomIntensity = zenMode ? 0.6 : (strobeActive ? 2.5 : 1.2)
  const chromaticOffset = zenMode ? 0.001 : (strobeActive ? 0.008 : 0.002)

  return (
    <EffectComposer multisampling={4}>
      <Bloom
        intensity={bloomIntensity}
        luminanceThreshold={0.2}
        luminanceSmoothing={0.9}
        mipmapBlur
        radius={0.8}
      />
      <ChromaticAberration
        blendFunction={BlendFunction.NORMAL}
        offset={new THREE.Vector2(chromaticOffset, chromaticOffset)}
        radialModulation={false}
        modulationOffset={0}
      />
      <Noise opacity={strobeActive ? 0.15 : (zenMode ? 0.02 : 0.05)} blendFunction={BlendFunction.OVERLAY} />
      <Vignette eskil={false} offset={0.1} darkness={strobeActive ? 0.4 : 0.6} />
      <ToneMapping mode={ToneMappingMode.ACES_FILMIC} />
    </EffectComposer>
  )
}

// Scene
function Scene({ strobeActive, strobeMode, mouse, audioData, themeIndex, zenMode, explosions, setExplosions }) {
  const { camera, gl } = useThree()

  useFrame((state) => {
    const t = state.clock.elapsedTime

    // Smooth camera movement
    const speed = zenMode ? 0.3 : 1
    const targetX = Math.sin(t * 0.1 * speed) * 2 + mouse.current.x * 3
    const targetY = Math.cos(t * 0.15 * speed) * 1 + mouse.current.y * 2
    const targetZ = 8 + Math.sin(t * 0.05 * speed) * 1

    camera.position.x += (targetX - camera.position.x) * 0.02
    camera.position.y += (targetY - camera.position.y) * 0.02
    camera.position.z += (targetZ - camera.position.z) * 0.02

    camera.lookAt(0, 0, 0)
  })

  // Handle clicks for explosions
  const handleClick = useCallback((event) => {
    const rect = gl.domElement.getBoundingClientRect()
    const x = ((event.clientX - rect.left) / rect.width) * 2 - 1
    const y = -((event.clientY - rect.top) / rect.height) * 2 + 1

    // Convert to 3D position
    const vector = new THREE.Vector3(x * 5, y * 3, 0)

    setExplosions(prev => [...prev, { id: Date.now(), position: vector }])
  }, [gl, setExplosions])

  useEffect(() => {
    gl.domElement.addEventListener('click', handleClick)
    return () => gl.domElement.removeEventListener('click', handleClick)
  }, [gl, handleClick])

  return (
    <>
      <color attach="background" args={['#030303']} />
      <fog attach="fog" args={['#030303', 5, 25]} />

      <ambientLight intensity={zenMode ? 0.05 : 0.1} />
      <pointLight position={[5, 5, 5]} intensity={strobeActive ? 30 : (zenMode ? 5 : 10)} color="#a855f7" />
      <pointLight position={[-5, -5, 5]} intensity={strobeActive ? 30 : (zenMode ? 5 : 10)} color="#06b6d4" />
      <pointLight position={[0, 0, 3]} intensity={strobeActive ? 20 : (zenMode ? 2 : 5)} color="#ffffff" />

      <NebulaBackground strobeActive={strobeActive} audioData={audioData} themeIndex={themeIndex} zenMode={zenMode} />
      <Portal strobeActive={strobeActive} />
      <Aurora strobeActive={strobeActive} zenMode={zenMode} />
      <GodRays strobeActive={strobeActive} />
      <LightPillars strobeActive={strobeActive} />

      <HolographicCore strobeActive={strobeActive} zenMode={zenMode} />
      <OrbitingParticles strobeActive={strobeActive} zenMode={zenMode} />
      <FloatingGeometries strobeActive={strobeActive} zenMode={zenMode} />
      <ConnectionLines strobeActive={strobeActive} />
      <EnergyWaves strobeActive={strobeActive} />

      {!zenMode && <DNAHelix strobeActive={strobeActive} />}
      {!zenMode && <MeteorShower strobeActive={strobeActive} />}
      {strobeActive && <Lightning strobeActive={strobeActive} />}

      {/* Click explosions */}
      {explosions.map(explosion => (
        <ClickExplosion
          key={explosion.id}
          position={explosion.position}
          onComplete={() => setExplosions(prev => prev.filter(e => e.id !== explosion.id))}
        />
      ))}

      <Effects strobeActive={strobeActive} zenMode={zenMode} />
    </>
  )
}

// Main App
function App() {
  const [strobeActive, setStrobeActive] = useState(false)
  const [strobeMode, setStrobeMode] = useState(0)
  const [entered, setEntered] = useState(false)
  const [showWarning, setShowWarning] = useState(true)
  const [titleText, setTitleText] = useState('COSKUN')
  const [isScrambling, setIsScrambling] = useState(true)
  const [audioData, setAudioData] = useState({ bass: 0, mid: 0, high: 0 })
  const [themeIndex, setThemeIndex] = useState(0)
  const [zenMode, setZenMode] = useState(false)
  const [showSettings, setShowSettings] = useState(false)
  const [audioEnabled, setAudioEnabled] = useState(false)
  const [explosions, setExplosions] = useState([])
  const [subtitleText, setSubtitleText] = useState('')
  const mouse = useRef({ x: 0, y: 0 })
  const audioContext = useRef(null)
  const analyser = useRef(null)
  const dataArray = useRef(null)

  const strobeModes = ['CHAOS', 'PULSE', 'WAVE', 'RAVE', 'MATRIX', 'COSMIC']
  const themeNames = Object.keys(themes)
  const currentTheme = themes[themeNames[themeIndex]]

  // Real audio reactivity with microphone
  const enableAudio = useCallback(async () => {
    try {
      const stream = await navigator.mediaDevices.getUserMedia({ audio: true })
      audioContext.current = new (window.AudioContext || window.webkitAudioContext)()
      analyser.current = audioContext.current.createAnalyser()
      analyser.current.fftSize = 256

      const source = audioContext.current.createMediaStreamSource(stream)
      source.connect(analyser.current)

      dataArray.current = new Uint8Array(analyser.current.frequencyBinCount)
      setAudioEnabled(true)
    } catch (err) {
      console.log('Audio not available:', err)
    }
  }, [])

  // Audio analysis loop
  useEffect(() => {
    if (!entered || !audioEnabled || !analyser.current) return

    const analyzeAudio = () => {
      if (analyser.current && dataArray.current) {
        analyser.current.getByteFrequencyData(dataArray.current)

        // Calculate bass, mid, high
        const bass = dataArray.current.slice(0, 10).reduce((a, b) => a + b, 0) / 10 / 255
        const mid = dataArray.current.slice(10, 50).reduce((a, b) => a + b, 0) / 40 / 255
        const high = dataArray.current.slice(50, 100).reduce((a, b) => a + b, 0) / 50 / 255

        setAudioData({ bass, mid, high })
      }
      requestAnimationFrame(analyzeAudio)
    }

    const animId = requestAnimationFrame(analyzeAudio)
    return () => cancelAnimationFrame(animId)
  }, [entered, audioEnabled])

  // Simulated audio reactivity fallback
  useEffect(() => {
    if (!entered || audioEnabled) return

    const interval = setInterval(() => {
      setAudioData({
        bass: Math.sin(Date.now() * 0.003) * 0.5 + 0.5,
        mid: Math.sin(Date.now() * 0.005) * 0.5 + 0.5,
        high: Math.sin(Date.now() * 0.007) * 0.5 + 0.5,
      })
    }, 50)

    return () => clearInterval(interval)
  }, [entered, audioEnabled])

  // Text scramble
  useEffect(() => {
    if (!entered || !isScrambling) return

    const finalText = 'COSKUN'
    const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()'
    let iteration = 0

    const interval = setInterval(() => {
      setTitleText(
        finalText
          .split('')
          .map((char, index) => {
            if (index < iteration) return finalText[index]
            return chars[Math.floor(Math.random() * chars.length)]
          })
          .join('')
      )

      iteration += 1 / 2

      if (iteration >= finalText.length) {
        setTitleText(finalText)
        setIsScrambling(false)
        clearInterval(interval)
      }
    }, 30)

    return () => clearInterval(interval)
  }, [entered, isScrambling])

  // Typewriter effect for subtitle
  useEffect(() => {
    if (!entered || isScrambling) return

    const fullText = 'DIGITAL EXPERIENCE'
    let index = 0

    const interval = setInterval(() => {
      setSubtitleText(fullText.slice(0, index + 1))
      index++

      if (index >= fullText.length) {
        clearInterval(interval)
      }
    }, 50)

    return () => clearInterval(interval)
  }, [entered, isScrambling])

  // Mouse tracking
  useEffect(() => {
    const handleMouseMove = (e) => {
      mouse.current.x = (e.clientX / window.innerWidth) * 2 - 1
      mouse.current.y = -(e.clientY / window.innerHeight) * 2 + 1
    }
    window.addEventListener('mousemove', handleMouseMove)
    return () => window.removeEventListener('mousemove', handleMouseMove)
  }, [])

  // Keyboard
  useEffect(() => {
    const handleKeyDown = (e) => {
      if (e.code === 'Space') {
        e.preventDefault()
        setStrobeActive(true)
      }
      if (e.code === 'KeyM') setStrobeMode((prev) => (prev + 1) % strobeModes.length)
      if (e.code === 'KeyR') setIsScrambling(true)
      if (e.code === 'KeyT') setThemeIndex((prev) => (prev + 1) % themeNames.length)
      if (e.code === 'KeyZ') setZenMode(prev => !prev)
      if (e.code === 'KeyS') setShowSettings(prev => !prev)
    }
    const handleKeyUp = (e) => {
      if (e.code === 'Space') setStrobeActive(false)
    }
    window.addEventListener('keydown', handleKeyDown)
    window.addEventListener('keyup', handleKeyUp)
    return () => {
      window.removeEventListener('keydown', handleKeyDown)
      window.removeEventListener('keyup', handleKeyUp)
    }
  }, [])

  const handleEnter = () => {
    setShowWarning(false)
    setTimeout(() => setEntered(true), 300)
  }

  if (showWarning) {
    return (
      <div className="warning-screen">
        <div className="warning-bg" />
        <div className="warning-content">
          <h1 className="warning-title">
            {'COSKUN'.split('').map((char, i) => (
              <span key={i} className="wletter">{char}</span>
            ))}
          </h1>
          <div className="warning-divider" />
          <p className="warning-text">PHOTOSENSITIVITY WARNING</p>
          <p className="warning-subtext">
            This experience contains intense flashing lights and strobe effects.
            <br />Click anywhere for particle explosions!
          </p>
          <button className="enter-button" onClick={handleEnter}>
            <span>ENTER</span>
          </button>
          <div className="feature-list">
            <span>3D Visuals</span>
            <span>•</span>
            <span>Audio Reactive</span>
            <span>•</span>
            <span>Interactive</span>
          </div>
        </div>
      </div>
    )
  }

  return (
    <div className={`app ${strobeActive ? 'strobe-active' : ''} ${zenMode ? 'zen-mode' : ''}`}
         style={{ '--theme-primary': currentTheme.primary, '--theme-secondary': currentTheme.secondary }}>
      <Canvas
        camera={{ position: [0, 0, 8], fov: 60 }}
        dpr={[1, 2]}
        gl={{
          antialias: true,
          alpha: false,
          powerPreference: 'high-performance',
          stencil: false,
        }}
      >
        <Suspense fallback={null}>
          <Scene
            strobeActive={strobeActive}
            strobeMode={strobeMode}
            mouse={mouse}
            audioData={audioData}
            themeIndex={themeIndex}
            zenMode={zenMode}
            explosions={explosions}
            setExplosions={setExplosions}
          />
        </Suspense>
      </Canvas>

      <div className="overlay">
        {/* Settings Panel */}
        {showSettings && (
          <div className="settings-panel">
            <h3>Settings</h3>
            <div className="setting-item">
              <label>Theme</label>
              <div className="theme-buttons">
                {themeNames.map((name, i) => (
                  <button
                    key={name}
                    className={`theme-btn ${themeIndex === i ? 'active' : ''}`}
                    onClick={() => setThemeIndex(i)}
                    style={{ background: themes[name].primary }}
                  />
                ))}
              </div>
            </div>
            <div className="setting-item">
              <label>Zen Mode</label>
              <button
                className={`toggle-btn ${zenMode ? 'active' : ''}`}
                onClick={() => setZenMode(!zenMode)}
              >
                {zenMode ? 'ON' : 'OFF'}
              </button>
            </div>
            <div className="setting-item">
              <label>Microphone</label>
              <button
                className={`toggle-btn ${audioEnabled ? 'active' : ''}`}
                onClick={enableAudio}
                disabled={audioEnabled}
              >
                {audioEnabled ? 'ACTIVE' : 'ENABLE'}
              </button>
            </div>
          </div>
        )}

        {/* Social Links */}
        <div className="social-links">
          <a href="#" className="social-link" title="Instagram">
            <svg viewBox="0 0 24 24" fill="currentColor">
              <path d="M12 2.163c3.204 0 3.584.012 4.85.07 3.252.148 4.771 1.691 4.919 4.919.058 1.265.069 1.645.069 4.849 0 3.205-.012 3.584-.069 4.849-.149 3.225-1.664 4.771-4.919 4.919-1.266.058-1.644.07-4.85.07-3.204 0-3.584-.012-4.849-.07-3.26-.149-4.771-1.699-4.919-4.92-.058-1.265-.07-1.644-.07-4.849 0-3.204.013-3.583.07-4.849.149-3.227 1.664-4.771 4.919-4.919 1.266-.057 1.645-.069 4.849-.069zm0-2.163c-3.259 0-3.667.014-4.947.072-4.358.2-6.78 2.618-6.98 6.98-.059 1.281-.073 1.689-.073 4.948 0 3.259.014 3.668.072 4.948.2 4.358 2.618 6.78 6.98 6.98 1.281.058 1.689.072 4.948.072 3.259 0 3.668-.014 4.948-.072 4.354-.2 6.782-2.618 6.979-6.98.059-1.28.073-1.689.073-4.948 0-3.259-.014-3.667-.072-4.947-.196-4.354-2.617-6.78-6.979-6.98-1.281-.059-1.69-.073-4.949-.073zm0 5.838c-3.403 0-6.162 2.759-6.162 6.162s2.759 6.163 6.162 6.163 6.162-2.759 6.162-6.163c0-3.403-2.759-6.162-6.162-6.162zm0 10.162c-2.209 0-4-1.79-4-4 0-2.209 1.791-4 4-4s4 1.791 4 4c0 2.21-1.791 4-4 4zm6.406-11.845c-.796 0-1.441.645-1.441 1.44s.645 1.44 1.441 1.44c.795 0 1.439-.645 1.439-1.44s-.644-1.44-1.439-1.44z"/>
            </svg>
          </a>
          <a href="#" className="social-link" title="Twitter">
            <svg viewBox="0 0 24 24" fill="currentColor">
              <path d="M18.244 2.25h3.308l-7.227 8.26 8.502 11.24H16.17l-5.214-6.817L4.99 21.75H1.68l7.73-8.835L1.254 2.25H8.08l4.713 6.231zm-1.161 17.52h1.833L7.084 4.126H5.117z"/>
            </svg>
          </a>
          <a href="#" className="social-link" title="GitHub">
            <svg viewBox="0 0 24 24" fill="currentColor">
              <path d="M12 0c-6.626 0-12 5.373-12 12 0 5.302 3.438 9.8 8.207 11.387.599.111.793-.261.793-.577v-2.234c-3.338.726-4.033-1.416-4.033-1.416-.546-1.387-1.333-1.756-1.333-1.756-1.089-.745.083-.729.083-.729 1.205.084 1.839 1.237 1.839 1.237 1.07 1.834 2.807 1.304 3.492.997.107-.775.418-1.305.762-1.604-2.665-.305-5.467-1.334-5.467-5.931 0-1.311.469-2.381 1.236-3.221-.124-.303-.535-1.524.117-3.176 0 0 1.008-.322 3.301 1.23.957-.266 1.983-.399 3.003-.404 1.02.005 2.047.138 3.006.404 2.291-1.552 3.297-1.23 3.297-1.23.653 1.653.242 2.874.118 3.176.77.84 1.235 1.911 1.235 3.221 0 4.609-2.807 5.624-5.479 5.921.43.372.823 1.102.823 2.222v3.293c0 .319.192.694.801.576 4.765-1.589 8.199-6.086 8.199-11.386 0-6.627-5.373-12-12-12z"/>
            </svg>
          </a>
        </div>

        <div className="center-content">
          <h1 className={`title ${zenMode ? 'zen-title' : ''}`}>
            {titleText.split('').map((char, i) => (
              <span key={i} className="letter" style={{ '--i': i }}>
                {char}
              </span>
            ))}
          </h1>
          <p className="subtitle">
            {subtitleText}
            <span className="cursor">|</span>
          </p>

          {zenMode && (
            <p className="zen-indicator">ZEN MODE</p>
          )}

          <div className="controls">
            <button
              className={`strobe-button ${strobeActive ? 'active' : ''}`}
              onMouseDown={() => setStrobeActive(true)}
              onMouseUp={() => setStrobeActive(false)}
              onMouseLeave={() => setStrobeActive(false)}
              onTouchStart={() => setStrobeActive(true)}
              onTouchEnd={() => setStrobeActive(false)}
            >
              <span className="button-label">HOLD FOR STROBE</span>
              <span className="button-hint">SPACE</span>
            </button>

            <div className="mode-toggle">
              <span className="mode-label">MODE</span>
              <button
                className="mode-button"
                onClick={() => setStrobeMode((prev) => (prev + 1) % strobeModes.length)}
              >
                {strobeModes[strobeMode]}
              </button>
            </div>

            <div className="mode-toggle">
              <span className="mode-label">THEME</span>
              <button
                className="mode-button theme-button"
                onClick={() => setThemeIndex((prev) => (prev + 1) % themeNames.length)}
              >
                {themeNames[themeIndex].toUpperCase()}
              </button>
            </div>
          </div>

          {/* Audio visualizer */}
          {audioEnabled && (
            <div className="audio-visualizer">
              <div className="audio-bar" style={{ height: `${audioData.bass * 100}%` }} />
              <div className="audio-bar" style={{ height: `${audioData.mid * 100}%` }} />
              <div className="audio-bar" style={{ height: `${audioData.high * 100}%` }} />
            </div>
          )}
        </div>

        <footer className="footer">
          <div className="footer-hints">
            <span>[SPACE] Strobe</span>
            <span>[M] Mode</span>
            <span>[T] Theme</span>
            <span>[Z] Zen</span>
            <span>[R] Scramble</span>
            <span>[S] Settings</span>
            <span>[CLICK] Explode</span>
          </div>
        </footer>
      </div>
    </div>
  )
}

export default App
