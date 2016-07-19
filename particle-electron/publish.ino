#define PIN D7

int state = 0;
int i;

void setup() {
    //tell the device we want to write to this pin
    pinMode(PIN, OUTPUT);
    i = 0;

    Spark.publish("spark-setup-no-arg");
    Spark.publish("spark-setup", "fixed-arg");
    Spark.publish("spark-setup", "with-ttl", 21600);
    Spark.publish("spark-setup", "with-ttl-explicitly-public", 21600, PUBLIC);

    Particle.publish("particle-setup-no-arg");
    Particle.publish("particle-setup", "fixed-arg");
    Particle.publish("particle-setup", "with-ttl", 21600);
    Particle.publish("particle-setup", "with-ttl-explicitly-public", 21600, PUBLIC);
}

void loop() {
    // alternate the PIN between high and low
    digitalWrite(PIN, (state) ? HIGH : LOW);

    // invert the state
    state = !state;

    // bump the publish counter
    if (i % 5 == 0) {
        Spark.publish("its-alive", String(i));
        delay(20);
        Spark.publish("spark-loop-no-arg");
        delay(20);
        Spark.publish("spark-loop", "fixed-arg");
        delay(20);
        Spark.publish("spark-loop", "with-ttl", 21600);
        delay(20);
        Spark.publish("spark-loop", "with-ttl-explicitly-public", 21600, PUBLIC);
        delay(20);
    }
    i += 1;

    // wait a second
    delay(900);
}
