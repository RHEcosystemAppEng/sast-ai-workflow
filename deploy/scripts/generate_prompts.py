#!/usr/bin/env python3
"""
Generate Kubernetes ConfigMap from prompt template files.
Used by the Makefile to create sast-ai-prompt-templates.yaml from src/templates/prompts/*.yaml
"""

import yaml
import os
import sys
from pathlib import Path


def main():
    """Generate ConfigMap from prompt template files."""
    # Path to prompt templates (relative to deploy directory)
    prompts_dir = Path('../src/templates/prompts')
    
    if not prompts_dir.exists():
        print(f'Error: Prompt templates directory not found at {prompts_dir.resolve()}')
        sys.exit(1)
    
    # Load all template files
    templates = {}
    yaml_files = sorted(prompts_dir.glob('*.yaml'))
    
    if not yaml_files:
        print(f'Error: No YAML template files found in {prompts_dir.resolve()}')
        sys.exit(1)
    
    print(f'Loading {len(yaml_files)} prompt template files...')
    
    for yaml_file in yaml_files:
        try:
            with open(yaml_file, 'r', encoding='utf-8') as f:
                data = yaml.safe_load(f)
                template = data.get('template', '')
                if template:
                    templates[yaml_file.stem] = template
                    print(f'  ✓ {yaml_file.name}')
                else:
                    print(f'  ✗ {yaml_file.name}: Missing or empty "template" key')
                    sys.exit(1)
        except Exception as e:
            print(f'  ✗ {yaml_file.name}: Error - {e}')
            sys.exit(1)
    
    # Create ConfigMap structure
    configmap = {
        'apiVersion': 'v1',
        'kind': 'ConfigMap',
        'metadata': {
            'name': 'sast-ai-prompt-templates',
            'labels': {
                'app': 'sast-ai'
            }
        },
        'data': templates
    }
    
    # Write ConfigMap to file
    output_file = 'tekton/sast-ai-prompt-templates.yaml'
    try:
        with open(output_file, 'w', encoding='utf-8') as f:
            # Write header comments
            f.write('# Generated from src/templates/prompts/*.yaml files\n')
            f.write('# To regenerate: make generate-prompts\n')
            f.write('# Single source of truth for all prompts\n\n')
            
            # Write YAML content
            yaml.dump(configmap, f, default_flow_style=False, 
                     allow_unicode=True, sort_keys=False)
        
        print(f'✅ ConfigMap generated successfully: {output_file}')
        
    except Exception as e:
        print(f'Error writing ConfigMap file: {e}')
        sys.exit(1)


if __name__ == '__main__':
    main() 
