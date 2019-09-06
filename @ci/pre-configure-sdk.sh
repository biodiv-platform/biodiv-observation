# THIS SCRIPT WILL RUN INITIALLY TO SETUP `~/.m2/settings.xml`

ENDPOINT=${ARTIFACTORY_ENDPOINT:-"http://localhost:8081/artifactory"}

cp @ci/sdk-pom.xml.template @ci/sdk-pom.xml

sed -i "s|__ARTIFACTORY__|$ENDPOINT|g" @ci/sdk-pom.xml

chmod +x @ci/pom-replacer
./@ci/pom-replacer target/sdk/pom.xml @ci/sdk-pom.xml
