contract kf_test5
{
	uint total = 1;
	mapping(uint => Info) public remarks;
	struct Info	{
		uint id;
		uint[] values;
		string message;
	}
	event Deposit(
		address indexed _from,
		bytes32 indexed _id,
		uint _value
	);
	function deposit(bytes32 _id) {
		Deposit(msg.sender, _id, msg.value);
	}
	function add(uint x, uint y) public returns (uint z) {
		total = x + y;
		z = total;
	}
	function subtract(uint x, uint y) public returns (uint z) {
		total = x - y;
		z = total;
	}
	function multiply(uint x, uint y) public returns (uint z) {
		total = x * y;
		z = total;
	}
	function divid(uint x, uint y) public returns (uint z) {
		total = x / y;
		z = total;
	}
	function getTotal() public returns (uint z) {
		z = total;
	}
	function call() public returns (string res) {
		res = "Hello";
	}
	function callName(string name) public returns (string res) {
		res = name;
	}
	function func(uint x) {
		total = x;
	}
	function getMessage(uint _id, uint[] _values, string _message) {
		remarks[_id] = Info({
			id: _id,
			values: _values,
			message: _message
		});
	}
	function getListTotal(uint[] _values) public returns (uint) {
		uint t = 0;
		for(uint i = 0; i < _values.length; i++) {
			t += _values[i];
		}
		total = t;
		return t;
	}
	function getListInfo(uint[] _values) public returns (uint[]) {
		uint total = 0;
		uint count = 0;
		for(uint i = 0; i < _values.length; i++) {
			total += _values[i];
			count ++;
		}
		uint[] memory res = new uint[](2);
		res[0] = total;
		res[1] = count;
		return res;
	}
}