#!/bin/sh

echo "GCF Executive Dashboard Test"
echo "============================="
echo ""
echo "Starting server on port 8080..."
guile -L src src/main.scm &
SERVER_PID=$!
echo "Server PID: $SERVER_PID"

sleep 5

echo ""
echo "Testing endpoints:"
echo ""

echo "1. Health check:"
curl -s http://localhost:8080/health 2>&1 || echo "FAILED"

echo ""
echo "2. Main dashboard:"
curl -s http://localhost:8080/ 2>&1 | grep -o "<title>.*</title>" || echo "FAILED"

echo ""
echo "3. Executive dashboard:"
curl -s http://localhost:8080/executive 2>&1 | grep -o "<title>.*</title>" || echo "FAILED"

echo ""
echo "4. API endpoint:"
curl -s http://localhost:8080/api/changes 2>&1 | head -c 100 || echo "FAILED"

echo ""
echo ""
echo "Stopping server..."
kill $SERVER_PID 2>/dev/null

echo "Test complete!"