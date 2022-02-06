import { IKnownApis } from "./cobolCallTargets";

export class MFUNIT_APIs implements IKnownApis {
    public url = "https,//www.microfocus.com/documentation/enterprise-developer/ed60/ED-VS2019/GUID-D6AC86CD-7222-46E6-94F9-0F7E78D21075.html";
    public name = "Micro Focus Unit Testing Framework Library Routines";
    public apis = new Map<string, string>([
        ["MFU_ASSERT_FAIL", "Signifies that a test case running under the Micro Focus Unit Testing framework has failed, and outputs a message to the test report."],
        ["MFUFMSG", "Signifies that a mainframe test case running under the Micro Focus Unit Testing framework has failed, and outputs a message to the test report."],
        ["MFU_ASSERT_FAIL_Z", "Signifies that a test case running under the Micro Focus Unit Testing framework has failed, and outputs a null-terminated message to the test report."],
        ["MFUFMSGZ", "Signifies that a mainframe test case running under the Micro Focus Unit Testing framework has failed, and outputs a null-terminated message to the test report."],
        ["MFUGETF", "Loads a file into memory, in preparation for running data-driven tests."],
        ["MFU_GET_FILE", "Loads a file into memory, in preparation for running data-driven tests."],
        ["MFUCATCP", "Copy file from catalog"],
        ["MFUCATCPZ", "Copy file from catalog"]
    ]);

    public snippets = new Map<string, string>([
        ["MFUFMSG", "call \"MFUFMSG\" using\r\n by reference ${1:any}\r\n by value ${2:msg-len}\r\nend-call"],
        ["MFU_ASSERT_FAIL_Z", "call \"MFU_ASSERT_FAIL_Z\" using\r\n by reference ${1:any}\r\nend-call"],
        ["MFUFMSGZ", "call \"MFUFMSGZ\" using\r\n by reference ${1:any}\r\nend-call"],
        ["MFUCATCP", "call \"MFUCATCP\" using\r\n by reference ${1:any}\r\n by value ${2:fn-len}\r\nend-call"],
        ["MFUCATCPZ", "call \"MFUCATCPZ\" using\r\n by reference ${1:any}\r\nend-call"],
        ["MFUGETF", "call \"MFUGETF\" using\r\n by reference ${1:get-filename}\r\n by reference ${2:data-ptr}\r\n by reference ${3:data-size}\r\nend-call"],
        ["MFU_GET_FILE", "call \"MFU_GET_FILE\" using\r\n by reference ${1:get-filename}\r\n by reference ${2:data-ptr}\r\n by reference ${3:data-size}\r\nend-call"]
    ]);

    public examples = new Map<string, string>([
        ["MFUFMSG", "call \"MFUFMSG\" using\r\n by reference any\r\n by value msg-len\r\nend-call"],
        ["MFU_ASSERT_FAIL_Z", "call \"MFU_ASSERT_FAIL_Z\" using\r\n by reference any\r\nend-call"],
        ["MFUFMSGZ", "call \"MFUFMSGZ\" using\r\n by reference any\r\nend-call"],
        ["MFUCATCP", "call \"MFUCATCP\" using\r\n by reference any\r\n by value fn-len\r\nend-call"],
        ["MFUCATCPZ", "call \"MFUCATCPZ\" using\r\n by reference any\r\nend-call"],
        ["MFUGETF", "call \"MFUGETF\" using\r\n by reference get-filename\r\n by reference data-ptr\r\n by reference data-size\r\nend-call"]
    ]);

}

