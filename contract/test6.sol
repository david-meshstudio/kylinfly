contract test6
{
	event SSOpt(
		bytes32 cmd,
		bytes32 c1,
		bytes32 c2
	);
	function Test1(bytes32 cmd, bytes32 c1, bytes32 c2) {
		SSOpt(cmd, c1, c2);
	}
}