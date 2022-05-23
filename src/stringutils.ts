export class SimpleStringBuilder {
    public Values: string[] = [];

    constructor(value?: string) {
        if (!SimpleStringBuilder.IsNullOrWhiteSpace(value as string)) {
            this.Values = new Array(value as string);
        }
    }

    private static IsNullOrWhiteSpace(value: string): boolean {
        try {
            if (value === null || value === "undefined") {
                return true;
            }

            return value.toString().replace(/\s/g, "").length < 1;
        }
        catch(e) {
            return false;
        }
    }

    public ToString() {
        return this.Values.join("");
    }

    public Append(value: string) {
        this.Values.push(value);
    }

    public AppendLine(value: string) {
        this.Values.push(`\r\n${value}`);
    }

    public Clear() {
        this.Values = [];
    }
}