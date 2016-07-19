#define PIN D7

int state = 0;
int i;

void setup() {
    //tell the device we want to write to this pin
    pinMode(PIN, OUTPUT);
    i = 0;
}

void loop() {
    // alternate the PIN between high and low
    digitalWrite(PIN, (state) ? HIGH : LOW);

    // invert the state
    state = !state;

    // bump the publish counter
    if (i % 5 == 0) {
        // Doesn't work. No idea why.
        //Spark.publish("spark-loop-no-arg");
        // Works fine. No idea why either.
        Spark.publish("its-alive", String(i));
    }
    i += 1;

    // wait a second
    delay(1000);
}
