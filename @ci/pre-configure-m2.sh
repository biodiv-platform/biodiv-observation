# THIS SCRIPT WILL RUN BEFORE SDK BUILD

ENDPOINT=${ARTIFACTORY_ENDPOINT:-"http://localhost:8081/artifactory"}
USERNAME=${ARTIFACTORY_USERNAME:-"admin"}
PASSWORD=${ARTIFACTORY_PASSWORD:-"password"}

cp @ci/settings.xml.template @ci/settings.xml

sed -i "s|__ARTIFACTORY__|$ENDPOINT|g" @ci/settings.xml
sed -i "s|__USERNAME__|$USERNAME|g"    @ci/settings.xml
sed -i "s|__PASSWORD__|$PASSWORD|g"    @ci/settings.xml

sed -i "s|__ARTIFACTORY__|$ENDPOINT|g" @ci/settings.xml

mv @ci/settings.xml ~/.m2/settings.xml

echo "️⚡️ global m2 settings configured"
