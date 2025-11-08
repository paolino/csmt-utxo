# Remote docker logs

An HTTP service (server and clients) to get docker logs from machines with no exposed ports.

## Features
- List containers from multiple remote Docker hosts.
- Fetch logs from remote Docker containers.
- Support for streaming logs.
- Secure authorization using tokens.
- Command-line interface for managing authorization tokens.
  
## Architecture

The system consists of two main components: a **proxy** and a **source** and an executable to issue access tokens.

- The **proxy** is a server that exposes an HTTP API to users. It receives requests for logs and forwards them to the appropriate **source**. It listen to responses from sources and forward them to the user.
- Each **source** is an HTTP client of the **proxy** that runs on the machine where containers logs are to be followed. It registers itself with the proxy and listens for logs requests. When it receives a request, it forwards it to the local docker daemon socket and forward back the response to the proxy.
- Access to the proxy is protected by tokens one for each source and one for the user.
- Access tokens are a stripped version of jwt.

## Deployment
- The proxy should be deployed on a machine with a public IP address to which both sources and user can  send HTTP requests
- Each source should be deployed on the machines where Docker is running and container logs needs to be followed.
- The auth CLI can be run from any machine.
- The proxy and sources can be deployed using Docker containers, as shown in the provided `docker-compose.yaml` files.
- Configuration files for both the proxy and sources should be created with the necessary settings, including URLs, tokens, and other parameters.

## Proxy user API
The proxy exposes the following HTTP endpoints:
- `GET /containers`: Retrieves a list of Docker containers from all registered sources.
- `GET /containers/{container_id}/logs`: Fetches logs for a specific container. It appends the source host to each log. All standard Docker log parameters are supported.
  - follow
  - stdout
  - stderr
  - since
  - timestamps
  - tail