#!/usr/bin/env bash

# Clear GitHub Actions Queue Script
# Cancels all queued workflow runs to unstick the pipeline

set -e

REPO="dsp-dr/guile-changeflow"

echo "🧹 Clearing GitHub Actions Queue"
echo "================================"

# Get count of queued runs
QUEUED_COUNT=$(gh run list --repo "$REPO" --status queued --json databaseId | jq '. | length')

if [ "$QUEUED_COUNT" -eq 0 ]; then
    echo "✅ No queued runs to clear"
    exit 0
fi

echo "Found $QUEUED_COUNT queued runs"
echo ""

# List queued runs by workflow
echo "Queued runs by workflow:"
gh run list --repo "$REPO" --status queued --json databaseId,workflowName | \
    jq -r '.[] | .workflowName' | sort | uniq -c

echo ""
echo "Cancelling all queued runs..."

# Cancel all queued runs
gh run list --repo "$REPO" --status queued --json databaseId | \
    jq -r '.[].databaseId' | \
    xargs -I {} gh run cancel {} --repo "$REPO"

echo ""
echo "✅ Queue cleared!"

# Optional: trigger new deployment
if [ "$1" = "--deploy" ]; then
    echo ""
    echo "🚀 Triggering new Cloudflare deployment..."
    gh workflow run "Deploy to Cloudflare Workers" --repo "$REPO"
    echo "✅ Deployment triggered"
fi