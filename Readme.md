# Template Module

The Template Module renders templates with the Handlebars render engine (based on Mustache).

## Installation
```
docker run --name <container name> \
           -v <path to templates volume>:/data/template \
           -v <path to transport volume>:/data/transport \
           -e NODE_HOST=<container name> \
           -e ZOOKEEPER_HOST=<container name of zookeeper> \
           simplifierdist.azurecr.io/template:<current version>
```

## HTTP API interface

### Rendering

#### Render Request
```
POST http://localhost:8080/client/2.0/template/<template folder>/<template name>

<json>
```

##### JSON body
The JSON body is an object with the names (respectively the aliases) 
of the defined parameters as its keys.
```json
{
    "<parameter name>": <parameter value>
}
```

#### Render Response
The response is a JSON object with key "Template" 
providing the rendered template as string.
```json
{
    "Template": "<rendered template>"
}
```