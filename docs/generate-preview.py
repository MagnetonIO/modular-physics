#!/usr/bin/env python3
"""
Generate social media preview image for Modular Physics
Creates a 1200x630px image for Open Graph and Twitter Cards
"""

from PIL import Image, ImageDraw, ImageFont
import os

# Create image with gradient background
width, height = 1200, 630
img = Image.new('RGB', (width, height), color='#2563eb')
draw = ImageDraw.Draw(img)

# Create gradient effect by overlaying rectangles
for i in range(height):
    # Gradient from blue to purple
    r = int(37 + (124 - 37) * (i / height))  # #2563eb to #7c3aed
    g = int(99 + (58 - 99) * (i / height))
    b = int(235 + (237 - 235) * (i / height))
    draw.rectangle([(0, i), (width, i+1)], fill=(r, g, b))

# Add dark overlay for text contrast
overlay = Image.new('RGBA', (width, height), (17, 24, 39, 180))
img = Image.alpha_composite(img.convert('RGBA'), overlay).convert('RGB')
draw = ImageDraw.Draw(img)

# Try to use system font, fallback to default
try:
    title_font = ImageFont.truetype("/System/Library/Fonts/Helvetica.ttc", 72)
    subtitle_font = ImageFont.truetype("/System/Library/Fonts/Helvetica.ttc", 32)
    equation_font = ImageFont.truetype("/System/Library/Fonts/Courier.ttc", 64)
    tagline_font = ImageFont.truetype("/System/Library/Fonts/Helvetica.ttc", 28)
    small_font = ImageFont.truetype("/System/Library/Fonts/Helvetica.ttc", 18)
except:
    # Fallback to default font
    title_font = ImageFont.load_default()
    subtitle_font = ImageFont.load_default()
    equation_font = ImageFont.load_default()
    tagline_font = ImageFont.load_default()
    small_font = ImageFont.load_default()

# Draw particle network effect
import random
particles = []
for _ in range(30):
    particles.append({
        'x': random.randint(0, width),
        'y': random.randint(0, height)
    })

# Draw connections between nearby particles
for i, p1 in enumerate(particles):
    for p2 in particles[i+1:]:
        dist = ((p1['x'] - p2['x'])**2 + (p1['y'] - p2['y'])**2)**0.5
        if dist < 200:
            draw.line([(p1['x'], p1['y']), (p2['x'], p2['y'])], 
                     fill=(255, 255, 255, 25), width=1)

# Draw particles
for p in particles:
    radius = random.randint(1, 3)
    draw.ellipse([(p['x']-radius, p['y']-radius), 
                  (p['x']+radius, p['y']+radius)], 
                 fill=(255, 255, 255, 150))

# Add text elements
# Title
draw.text((width/2, 180), "Modular Physics", 
          font=title_font, fill='white', anchor='mm')

# Subtitle
draw.text((width/2, 250), "Compositional Framework for Fundamental Laws", 
          font=subtitle_font, fill=(255, 255, 255, 230), anchor='mm')

# Equation box
box_x, box_y, box_w, box_h = 350, 320, 500, 120
draw.rectangle([(box_x, box_y), (box_x + box_w, box_y + box_h)], 
               fill=(255, 255, 255, 38), outline=(255, 255, 255, 76), width=2)

# Main equation
draw.text((width/2, 380), "E = Ic²", 
          font=equation_font, fill='white', anchor='mm')

# Tagline
draw.text((width/2, 490), "Laws That Compose, Not Unify", 
          font=tagline_font, fill=(255, 255, 255, 216), anchor='mm')

# Four laws at bottom
laws = [
    ('🔬', 'Size-Aware'),
    ('🌡️', 'Thermal'),
    ('🌌', 'Geometric'),
    ('🪐', 'Gravitational')
]

start_x = 240
spacing = 180
for i, (icon, label) in enumerate(laws):
    x = start_x + (i * spacing)
    y = 550
    
    # Draw circle background
    draw.ellipse([(x-30, y-30), (x+30, y+30)], 
                 fill=(255, 255, 255, 25))
    
    # Draw label (skip emoji as PIL has issues with them)
    draw.text((x, y+40), label, 
              font=small_font, fill=(255, 255, 255, 204), anchor='mm')

# GitHub reference
draw.text((width-50, height-25), "github.com/MagnetonIO/modular-physics", 
          font=small_font, fill=(255, 255, 255, 153), anchor='rm')

# Save the image
img.save('social-preview.png', 'PNG', quality=95, optimize=True)
print("Social preview image generated: social-preview.png")