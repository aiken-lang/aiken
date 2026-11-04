use super::Project;

#[test]
fn native_function_matches_golden() {
    let project = Project::fixture("positive/native_function");
    project.generate();
    project.assert_golden("positive/native_function");
}

#[test]
fn universal_validator_matches_golden() {
    let project = Project::fixture("positive/universal_validator");
    project.generate();
    project.assert_golden("positive/universal_validator");
}

#[test]
fn falsified_property_still_generates_matching_golden() {
    let project = Project::fixture("positive/falsified");
    project.generate();
    project.assert_golden("positive/falsified");
}

#[test]
fn recursive_data_matches_golden() {
    let project = Project::fixture("positive/recursive_data");
    project.generate();
    project.assert_golden("positive/recursive_data");
}
