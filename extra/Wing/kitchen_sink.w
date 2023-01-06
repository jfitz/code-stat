bring cloud;
bring fs;

struct DenyListRule {
  package_name: str;
  version: str?;
  reason: str;
}

struct DenyListProps {
  rules: MutArray<DenyListRule >;
}

resource DenyList {
  _bucket: cloud.Bucket;
  _object_key: str;

  init(props: DenyListProps) {
    this._bucket = cloud.Bucket();
    this._object_key = "deny-list.json";

    let rules_dir = this._write_to_file(props.rules, this._object_key);
    this._bucket.upload("${rules_dir}/*/**", prune: true, retain_on_delete: true);
  }

  _write_to_file(list: MutArray<DenyListRule>,  filename: str): str {
    let tmpdir = fs.mkdtemp();
    let filepath = "${tmpdir}/${filename}";
    let map = MutMap<DenyListRule>(); 
    for rule in list {
      let suffix = DenyList._maybe_suffix(rule.version);
      let path = "${rule.package_name}${suffix}";
      map[path] = rule;
    }
    fs.write_json(filepath, map);
    return tmpdir;
  }

  inflight rules: MutMap<DenyListRule>?; 

  inflight init() {
    // this._bucket is already initialized by the capture mechanic!
    this.rules = this._bucket.get(this._object_key) ?? MutMap<DenyListRule>(); 
  }

  public inflight lookup(name: str, version: str): DenyListRule? {
    return this.rules[name] ?? this.rules["${name}/v${version}"];
  }

  static _maybe_suffix(version: str?): str {
    if version {
      return "/v${version}";
    } else {
      return "";
    }
  }
}

let deny_list = DenyList();
let filter_fn = inflight (event: cloud.QueueEvent) => {
  let package_name = event.data["package_name"];
  let version = event.data["version"];
  let reason = event.data["reason"];
  if deny_list.lookup(package_name, version) {
    print("Package rejected: ${package_name}");
  } else {
    print("Package accepted: ${package_name}");
  }
};

queue = cloud.Queue();
filter = cloud.Function(filter_fn);
queue.add_consumer(filter);