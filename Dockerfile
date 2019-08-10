FROM node:11.6.0-alpine AS builder
COPY . ./piramida-frontend
WORKDIR /piramida-frontend
RUN npm i
RUN $(npm bin)/elm make src/Main.elm --output=build/index.html

FROM nginx:1.15.8-alpine
COPY --from=builder /piramida-frontend/build/index.html /usr/share/nginx/html

