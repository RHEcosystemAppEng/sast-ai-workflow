# Deploy Scripts

This directory contains utility scripts used during the deployment process.

## Scripts

### `generate_prompts.py`

Generates the Kubernetes ConfigMap (`tekton/sast-ai-prompt-templates.yaml`) from the prompt template files located in `../src/templates/prompts/*.yaml`.

**Usage:**
```bash
# From deploy/ directory
make generate-prompts
# or directly
python3 scripts/generate_prompts.py
```

**Purpose:**
- Maintains single source of truth for prompts
- Eliminates duplication between config.py and ConfigMap
- Ensures ConfigMap is always up-to-date with template files

**Requirements:**
- Python 3.6+
- PyYAML (`pip install PyYAML`)

## Adding New Scripts

When adding new deployment utility scripts:

1. Place them in this `scripts/` directory
2. Make them executable: `chmod +x script_name.py`
3. Add corresponding Makefile targets in `../Makefile`
4. Document them in this README
5. Follow the existing error handling patterns 
