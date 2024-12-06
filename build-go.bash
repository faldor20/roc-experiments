CGO_ENABLED=0 GOOS=linux GOARCH=amd64 \
go build -o app \
    -ldflags="-w -s" \
    -gcflags="-N -l" \
    -trimpath \
    -a -installsuffix cgo \
    main.go
