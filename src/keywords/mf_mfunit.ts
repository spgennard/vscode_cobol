import { IKnownApis } from "./cobolCallTargets";

export class MFUNIT_APIs implements IKnownApis {
    public url = "https://www.microfocus.com/documentation/enterprise-developer/ed60/ED-VS2019/GUID-D6AC86CD-7222-46E6-94F9-0F7E78D21075.html";
    public name = "Micro Focus Unit Testing Framework Library Routines";
    public apis = new Map<string, string[]>([
        ["MFU_ASSERT_FAIL", ["Signifies that a test case running under the Micro Focus Unit Testing framework has failed, and outputs a message to the test report."]],
        ["MFUFMSG", ["Signifies that a mainframe test case running under the Micro Focus Unit Testing framework has failed, and outputs a message to the test report."]],
        ["MFU_ASSERT_FAIL_Z", ["Signifies that a test case running under the Micro Focus Unit Testing framework has failed, and outputs a null-terminated message to the test report."]],
        ["MFUFMSGZ", ["Signifies that a mainframe test case running under the Micro Focus Unit Testing framework has failed, and outputs a null-terminated message to the test report."]],
        ["MFUGETF", ["Loads a file into memory, in preparation for running data-driven tests."]],
        ["MFU_GET_FILE", ["Loads a file into memory, in preparation for running data-driven tests."]],
        ["MFUCATCP", ["Copy file from catalog"]],
        ["MFUCATCPZ", ["Copy file from catalog"]]
    ]);

    public snippets = new Map<string, string[]>([
        ["MFUFMSG", ["call \"MFUFMSG\" using", " by reference ${1:any}", " by value ${2:msg-len}", "end-call"]],
        ["MFU_ASSERT_FAIL_Z", ["call \"MFU_ASSERT_FAIL_Z\" using", " by reference ${1:any}", "end-call"]],
        ["MFUFMSGZ", ["call \"MFUFMSGZ\" using", " by reference ${1:any}", "end-call"]],
        ["MFUCATCP", ["call \"MFUCATCP\" using", " by reference ${1:any}", " by value ${2:fn-len}", "end-call"]],
        ["MFUCATCPZ", ["call \"MFUCATCPZ\" using", " by reference ${1:any}", "end-call"]],
        ["MFUGETF", ["call \"MFUGETF\" using", " by reference ${1:get-filename}", " by reference ${2:data-ptr}", " by reference ${3:data-size}", "end-call"]],
        ["MFU_GET_FILE", ["call \"MFU_GET_FILE\" using", " by reference ${1:get-filename}", " by reference ${2:data-ptr}", " by reference ${3:data-size}", "end-call"]]
    ]);

    public examples = new Map<string, string[]>([
        ["MFUFMSG", ["call \"MFUFMSG\" using", " by reference any", " by value msg-len", "end-call"]],
        ["MFU_ASSERT_FAIL_Z", ["call \"MFU_ASSERT_FAIL_Z\" using", " by reference any", "end-call"]],
        ["MFUFMSGZ", ["call \"MFUFMSGZ\" using", " by reference any", "end-call"]],
        ["MFUCATCP", ["call \"MFUCATCP\" using", " by reference any", " by value fn-len", "end-call"]],
        ["MFUCATCPZ", ["call \"MFUCATCPZ\" using", " by reference any", "end-call"]],
        ["MFUGETF", ["call \"MFUGETF\" using", " by reference get-filename", " by reference data-ptr", " by reference data-size", "end-call"]]
    ]);

}

