import { useState, useEffect, useRef, useMemo, Suspense, useCallback } from 'react'
import { Canvas, useFrame, useThree, extend } from '@react-three/fiber'
import { shaderMaterial, useTexture, Text, Float } from '@react-three/drei'
import { EffectComposer, Bloom, ChromaticAberration, Vignette, Noise, ToneMapping } from '@react-three/postprocessing'
import { BlendFunction, ToneMappingMode } from 'postprocessing'
import * as THREE from 'three'
import './App.css'

// Fractal noise shader for nebula effect
const NebulaMaterial = shaderMaterial(
  {
    uTime: 0,
    uStrobe: 0,
    uMouse: new THREE.Vector2(0.5, 0.5),
    uResolution: new THREE.Vector2(1, 1),
    uBass: 0,
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

      float t = uTime * 0.15;

      // Multi-layer nebula
      float n1 = fbm(vec3(uv * 3.0, t));
      float n2 = fbm(vec3(uv * 5.0 + 100.0, t * 1.3));
      float n3 = fbm(vec3(uv * 8.0 + 200.0, t * 0.7));

      float nebula = n1 * 0.5 + n2 * 0.3 + n3 * 0.2;
      nebula = nebula * 0.5 + 0.5;

      // Color palette
      float hue1 = 0.75 + sin(t * 0.5) * 0.1; // Purple
      float hue2 = 0.55 + cos(t * 0.3) * 0.1; // Cyan
      float hue3 = 0.85; // Pink

      vec3 col1 = hsv2rgb(vec3(hue1, 0.8, 0.6));
      vec3 col2 = hsv2rgb(vec3(hue2, 0.7, 0.5));
      vec3 col3 = hsv2rgb(vec3(hue3, 0.9, 0.7));

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

// Nebula background
function NebulaBackground({ strobeActive, audioData }) {
  const material = useRef()
  const { viewport } = useThree()

  useFrame((state) => {
    if (material.current) {
      material.current.uTime = state.clock.elapsedTime
      material.current.uStrobe = strobeActive ? 1.0 : 0.0
      material.current.uBass = audioData?.bass || 0
    }
  })

  return (
    <mesh position={[0, 0, -20]} scale={[viewport.width * 2, viewport.height * 2, 1]}>
      <planeGeometry args={[1, 1, 1, 1]} />
      <nebulaMaterial ref={material} />
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

// Central holographic shape
function HolographicCore({ strobeActive }) {
  const mesh = useRef()
  const material = useRef()

  useFrame((state) => {
    if (mesh.current) {
      mesh.current.rotation.x = state.clock.elapsedTime * 0.2
      mesh.current.rotation.y = state.clock.elapsedTime * 0.3

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
      <octahedronGeometry args={[1.5, 0]} />
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

// Orbiting particles with trails
function OrbitingParticles({ strobeActive }) {
  const particles = useRef()
  const count = 500
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
  }, [])

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

        const angle = 0.01 * (strobeActive ? 3 : 1)
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
        size={0.03}
        color={strobeActive ? '#ffffff' : '#a855f7'}
        transparent
        opacity={0.8}
        blending={THREE.AdditiveBlending}
        sizeAttenuation
      />
    </points>
  )
}

// Floating geometric shapes
function FloatingGeometries({ strobeActive }) {
  const group = useRef()

  const geometries = useMemo(() => {
    return Array.from({ length: 15 }, (_, i) => ({
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
      type: ['tetrahedron', 'octahedron', 'icosahedron'][Math.floor(Math.random() * 3)],
      color: new THREE.Color().setHSL(Math.random() * 0.3 + 0.7, 0.8, 0.5),
    }))
  }, [])

  useFrame((state) => {
    if (group.current) {
      group.current.children.forEach((mesh, i) => {
        const data = geometries[i]
        const t = state.clock.elapsedTime * data.speed

        mesh.rotation.x = data.rotation.x + t
        mesh.rotation.y = data.rotation.y + t * 0.7
        mesh.position.y = data.position.y + Math.sin(t * 2) * 0.3

        if (strobeActive) {
          mesh.material.emissiveIntensity = 2 + Math.sin(t * 20)
        } else {
          mesh.material.emissiveIntensity = 0.5
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
          <meshStandardMaterial
            color={geo.color}
            emissive={geo.color}
            emissiveIntensity={0.5}
            wireframe
            transparent
            opacity={0.6}
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
function Effects({ strobeActive }) {
  const bloomIntensity = strobeActive ? 2.5 : 1.2
  const chromaticOffset = strobeActive ? 0.008 : 0.002

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
      <Noise opacity={strobeActive ? 0.15 : 0.05} blendFunction={BlendFunction.OVERLAY} />
      <Vignette eskil={false} offset={0.1} darkness={strobeActive ? 0.4 : 0.6} />
      <ToneMapping mode={ToneMappingMode.ACES_FILMIC} />
    </EffectComposer>
  )
}

// Scene
function Scene({ strobeActive, strobeMode, mouse, audioData }) {
  const { camera } = useThree()

  useFrame((state) => {
    const t = state.clock.elapsedTime

    // Smooth camera movement
    const targetX = Math.sin(t * 0.1) * 2 + mouse.current.x * 3
    const targetY = Math.cos(t * 0.15) * 1 + mouse.current.y * 2
    const targetZ = 8 + Math.sin(t * 0.05) * 1

    camera.position.x += (targetX - camera.position.x) * 0.02
    camera.position.y += (targetY - camera.position.y) * 0.02
    camera.position.z += (targetZ - camera.position.z) * 0.02

    camera.lookAt(0, 0, 0)
  })

  return (
    <>
      <color attach="background" args={['#030303']} />
      <fog attach="fog" args={['#030303', 5, 25]} />

      <ambientLight intensity={0.1} />
      <pointLight position={[5, 5, 5]} intensity={strobeActive ? 30 : 10} color="#a855f7" />
      <pointLight position={[-5, -5, 5]} intensity={strobeActive ? 30 : 10} color="#06b6d4" />
      <pointLight position={[0, 0, 3]} intensity={strobeActive ? 20 : 5} color="#ffffff" />

      <NebulaBackground strobeActive={strobeActive} audioData={audioData} />
      <GodRays strobeActive={strobeActive} />
      <LightPillars strobeActive={strobeActive} />

      <HolographicCore strobeActive={strobeActive} />
      <OrbitingParticles strobeActive={strobeActive} />
      <FloatingGeometries strobeActive={strobeActive} />
      <ConnectionLines strobeActive={strobeActive} />
      <EnergyWaves strobeActive={strobeActive} />

      <Effects strobeActive={strobeActive} />
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
  const mouse = useRef({ x: 0, y: 0 })
  const audioContext = useRef(null)
  const analyser = useRef(null)

  const strobeModes = ['CHAOS', 'PULSE', 'WAVE', 'RAVE']

  // Simulated audio reactivity
  useEffect(() => {
    if (!entered) return

    const interval = setInterval(() => {
      setAudioData({
        bass: Math.sin(Date.now() * 0.003) * 0.5 + 0.5,
        mid: Math.sin(Date.now() * 0.005) * 0.5 + 0.5,
        high: Math.sin(Date.now() * 0.007) * 0.5 + 0.5,
      })
    }, 50)

    return () => clearInterval(interval)
  }, [entered])

  // Text scramble
  useEffect(() => {
    if (!entered || !isScrambling) return

    const finalText = 'COSKUN'
    const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
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
          <h1 className="warning-title">COSKUN</h1>
          <div className="warning-divider" />
          <p className="warning-text">PHOTOSENSITIVITY WARNING</p>
          <p className="warning-subtext">
            This experience contains intense flashing lights and strobe effects.
          </p>
          <button className="enter-button" onClick={handleEnter}>
            <span>ENTER</span>
          </button>
        </div>
      </div>
    )
  }

  return (
    <div className={`app ${strobeActive ? 'strobe-active' : ''}`}>
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
          />
        </Suspense>
      </Canvas>

      <div className="overlay">
        <nav className="nav">
          <a href="#" className="nav-link">ABOUT</a>
          <a href="#" className="nav-link">WORK</a>
          <a href="#" className="nav-link">CONTACT</a>
        </nav>

        <div className="center-content">
          <h1 className="title">
            {titleText.split('').map((char, i) => (
              <span key={i} className="letter" style={{ '--i': i }}>
                {char}
              </span>
            ))}
          </h1>
          <p className="subtitle">DIGITAL EXPERIENCE</p>

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
          </div>
        </div>

        <footer className="footer">
          <div className="footer-hints">
            <span>[SPACE] Strobe</span>
            <span>[M] Mode</span>
            <span>[R] Scramble</span>
          </div>
        </footer>
      </div>
    </div>
  )
}

export default App
