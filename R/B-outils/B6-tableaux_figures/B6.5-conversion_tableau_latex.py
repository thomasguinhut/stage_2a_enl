import pandas as pd
import numpy as np


def read_csv_file(filepath):
    """Lit le fichier CSV et le traite"""
    try:
        # Lire le fichier CSV avec pandas
        df = pd.read_csv(filepath, sep=';')
        return df
    except Exception as e:
        print(f"Erreur lors de la lecture du fichier: {e}")
        return None


def format_value(value):
    """Formate une valeur avec une décimale, en évitant -0.0"""
    if pd.isna(value):
        return "0.0"

    rounded_val = round(float(value), 1)
    if rounded_val == -0.0:
        rounded_val = 0.0
    return f"{rounded_val:.1f}"


def create_latex_table(df, scenario_num=1):
    """Crée le tableau LaTeX à partir du DataFrame avec des couleurs pour les valeurs"""
    # Mapping de base des noms dans le CSV vers les noms du tableau
    estimator_mapping = {
        'monomode_expansion': 'Mono',
        'multimode_expansion': 'Multi',
        '1a': '1.a',
        '1aprime': "1.a'",
        '1b': '1.b',
        '2a': '2.a',
        '2aprime': "2.a'",
        '3a': '3.a',
        '3aprime': "3.a'",
        '3b': '3.b',
        '3bprime': "3.b'",
        '4': '4'
    }
    # Mapping spécial pour "Sans non-réponse" si 3 et 3prime existent
    unique_estimators = df['estimateur'].unique()
    has_3_single = '3' in unique_estimators
    has_3prime_single = '3prime' in unique_estimators
    estimator_mapping_sans_nr = estimator_mapping.copy()
    if has_3_single:
        estimator_mapping_sans_nr['3'] = '3'
    if has_3prime_single:
        estimator_mapping_sans_nr['3prime'] = "3'"
    method_mapping = {
        'sans_nr': 'Sans non-réponse',
        'cnr_exacte': 'Avec probas de réponse exactes',
        'sans_grh': 'Avec probas de réponse estimées',
        'avec_grh': 'Avec probas estimées et GRH'
    }
    year_mapping = {
        'y_1_': 'Y1',
        'y_2_': 'Y2',
        'y_3_': 'Y3'
    }
    # Ordre des estimateurs (fixe pour toutes les méthodes sauf "Sans non-réponse")
    estimator_order_default = ['Mono', 'Multi', '1.a', "1.a'",
                               '1.b', '2.a', "2.a'", '3.a', "3.a'", '3.b', "3.b'", '4']
    # Ordre spécial pour "Sans non-réponse" si 3 et 3prime existent
    estimator_order_sans_nr = ['Mono', 'Multi',
                               '1.a', "1.a'", '1.b', '2.a', "2.a'"]
    if has_3_single:
        estimator_order_sans_nr.append('3')
    else:
        estimator_order_sans_nr.extend(['3.a', '3.b'])
    if has_3prime_single:
        estimator_order_sans_nr.append("3'")
    else:
        estimator_order_sans_nr.extend(["3.a'", "3.b'"])
    estimator_order_sans_nr.append('4')
    method_order = ['Sans non-réponse', 'Avec probas de réponse exactes',
                    'Avec probas de réponse estimées', 'Avec probas estimées et GRH']
    year_order = ['Y1',
                  'Y2', 'Y3']
    # Début du tableau LaTeX avec les définitions de couleurs
    latex_content = [
        r"\begin{table}[H]",
        r"\vspace{-0.4cm}",
        r"\definecolor{color1}{HTML}{5D6D7E}",
        r"\definecolor{color2}{HTML}{E74C3C}",
        r"\definecolor{color3}{HTML}{1E8449}",
        r"\begin{tabularx}{\textwidth}{@{}cXcccccc@{}}",
        r"\toprule",
        f"\\multicolumn{{2}}{{c}}{{\\textbf{{Scénario {scenario_num}}}}} & \\multicolumn{{2}}{{c}}{{$Y_1$ (exponentielle)}} & \\multicolumn{{2}}{{c}}{{$Y_2$ (parabolique)}} & \\multicolumn{{2}}{{c}}{{$Y_3$ (normale)}} \\\\",
        r"\cmidrule(lr){3-4}\cmidrule(lr){5-6}\cmidrule(l){7-8}",
        r"\multicolumn{2}{c}{Estimateur} & Biais & CV(EQM) & Biais & CV(EQM) & Biais & CV(EQM) \\",
        r"\midrule"
    ]
    # Pour chaque méthode
    for method in method_order:
        # Trouver la méthode correspondante dans le CSV
        csv_method = None
        for csv_key, latex_method in method_mapping.items():
            if latex_method == method:
                csv_method = csv_key
                break
        if csv_method is None:
            continue
        # Filtrer les données pour cette méthode
        method_data = df[df['methode'] == csv_method]
        # Choisir le mapping et l'ordre appropriés selon la méthode
        if method == 'Sans non-réponse':
            current_estimator_mapping = estimator_mapping_sans_nr
            current_estimator_order = estimator_order_sans_nr
        else:
            current_estimator_mapping = estimator_mapping
            current_estimator_order = estimator_order_default
        # Calculer le nombre de lignes pour multirow
        num_estimators = len(current_estimator_order)
        # Préparer la première ligne de la méthode
        first_row_prefix = f"\\multirow{{{num_estimators}}}{{*}}{{\\rotatebox{{90}}{{\\centering {method}}}}} & {current_estimator_order[0]} & "
        # Pour chaque estimateur
        for i, estimator in enumerate(current_estimator_order):
            # Trouver l'estimateur correspondant dans le CSV
            csv_estimator = None
            for csv_key, latex_est in current_estimator_mapping.items():
                if latex_est == estimator:
                    csv_estimator = csv_key
                    break
            if csv_estimator is None:
                # Si l'estimateur n'existe pas, mettre des 0 colorés
                if i == 0:
                    zero_cell = r"\textcolor{color1}{0.0}/\textcolor{color2}{0.0}/\textcolor{color3}{0.0}"
                    latex_content.append(
                        first_row_prefix + zero_cell + " & " + zero_cell + " & " + zero_cell + " & " + zero_cell + " & " + zero_cell + " & " + zero_cell + " \\\\")
                else:
                    zero_cell = r"\textcolor{color1}{0.0}/\textcolor{color2}{0.0}/\textcolor{color3}{0.0}"
                    latex_content.append(
                        f"& {estimator} & " + zero_cell + " & " + zero_cell + " & " + zero_cell + " & " + zero_cell + " & " + zero_cell + " & " + zero_cell + " \\\\")
                continue
            # Filtrer les données pour cet estimateur
            estimator_data = method_data[method_data['estimateur']
                                         == csv_estimator]
            row_values = []
            # Pour chaque année et métrique
            for year in year_order:
                csv_year = None
                for csv_key, latex_year in year_mapping.items():
                    if latex_year == year:
                        csv_year = csv_key
                        break
                if csv_year is None:
                    zero_cell = r"\textcolor{color1}{0.0}/\textcolor{color2}{0.0}/\textcolor{color3}{0.0}"
                    row_values.extend([zero_cell, zero_cell])
                    continue
                year_data = estimator_data[estimator_data['y'] == csv_year]
                # BR (biais_relatif)
                br_values = {"total": "0.0",
                             "strate_A": "0.0", "strate_B": "0.0"}
                for _, row in year_data.iterrows():
                    ensemble = row['ensemble']
                    if ensemble in br_values:
                        br_values[ensemble] = format_value(
                            row['biais_relatif_abs'])
                br_cell = f"\\textcolor{{color1}}{{{br_values['total']}}}/\\textcolor{{color2}}{{{br_values['strate_A']}}}/\\textcolor{{color3}}{{{br_values['strate_B']}}}"
                # CV(EQM) (cv_reqm)
                cv_values = {"total": "0.0",
                             "strate_A": "0.0", "strate_B": "0.0"}
                for _, row in year_data.iterrows():
                    ensemble = row['ensemble']
                    if ensemble in cv_values:
                        cv_values[ensemble] = format_value(row['cv_reqm'])
                cv_cell = f"\\textcolor{{color1}}{{{cv_values['total']}}}/\\textcolor{{color2}}{{{cv_values['strate_A']}}}/\\textcolor{{color3}}{{{cv_values['strate_B']}}}"
                row_values.extend([br_cell, cv_cell])
            # Construire la ligne
            if i == 0:
                # Première ligne de la méthode
                latex_content.append(first_row_prefix +
                                     " & ".join(row_values) + " \\\\")
            else:
                # Lignes suivantes
                latex_content.append(
                    f"& {estimator} & " + " & ".join(row_values) + " \\\\")
        # Ajouter midrule après chaque méthode sauf la dernière
        if method != method_order[-1]:
            latex_content.append(r"\midrule")
    # Fin du tableau
    latex_content.extend([
        r"\bottomrule",
        r"\end{tabularx}",
        r"\caption{Valeur absolue des biais et coefficients de variation des EQM de chaque estimateur, selon le scénario 3 de réponse (en \%)}",
        r"\end{table}"
    ])
    return "\n".join(latex_content)


def main():
    # Nom du fichier CSV (ajustez selon votre fichier)
    csv_filename = "test_3.csv"

    # Lire le fichier CSV
    df = read_csv_file(csv_filename)

    if df is not None:
        print("Aperçu des données:")
        print(df.head())
        print(f"\nColonnes disponibles: {list(df.columns)}")
        print(f"Méthodes uniques: {df['methode'].unique()}")
        print(f"Estimateurs uniques: {df['estimateur'].unique()}")
        print(f"Années uniques: {df['y'].unique()}")
        print(f"Ensembles uniques: {df['ensemble'].unique()}")

        # Générer le tableau LaTeX
        latex_table = create_latex_table(df, scenario_num=1)

        # Sauvegarder dans un fichier
        output_filename = "tableau_latex_scenario_1.tex"
        with open(output_filename, 'w', encoding='utf-8') as f:
            f.write(latex_table)

        print(f"\nTableau LaTeX généré et sauvegardé dans '{output_filename}'")
        print("\nAperçu du tableau:")
        print(latex_table)
    else:
        print("Impossible de lire le fichier CSV.")


if __name__ == "__main__":
    main()
